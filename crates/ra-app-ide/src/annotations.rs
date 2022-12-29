use hir::{HasSource, InFile, Semantics};
use ide_db::{
    base_db::{FileId, FilePosition, FileRange},
    defs::Definition,
    helpers::visit_file_defs,
    RootDatabase,
};
use syntax::{ast::HasName, AstNode, TextRange};

use crate::{
    fn_references::find_all_methods,
    goto_implementation::goto_implementation,
    references::find_all_refs,
    runnables::{runnables, Runnable},
    NavigationTarget, RunnableKind,
};

// Feature: Annotations
//
// Provides user with annotations above items for looking up references or impl blocks
// and running/debugging binaries.
//
// image::https://user-images.githubusercontent.com/48062697/113020672-b7c34f00-917a-11eb-8f6e-858735660a0e.png[]
#[derive(Debug)]
pub struct Annotation {
    pub range: TextRange,
    pub kind: AnnotationKind,
}

#[derive(Debug)]
pub enum AnnotationKind {
    Runnable(Runnable),
    HasImpls { position: FilePosition, data: Option<Vec<NavigationTarget>> },
    HasReferences { position: FilePosition, data: Option<Vec<FileRange>> },
}

pub struct AnnotationConfig {
    pub binary_target: bool,
    pub annotate_runnables: bool,
    pub annotate_impls: bool,
    pub annotate_references: bool,
    pub annotate_method_references: bool,
    pub annotate_enum_variant_references: bool,
}

pub(crate) fn annotations(
    db: &RootDatabase,
    config: &AnnotationConfig,
    file_id: FileId,
) -> Vec<Annotation> {
    let mut annotations = Vec::default();

    if config.annotate_runnables {
        for runnable in runnables(db, file_id) {
            if should_skip_runnable(&runnable.kind, config.binary_target) {
                continue;
            }

            let range = runnable.nav.focus_or_full_range();

            annotations.push(Annotation { range, kind: AnnotationKind::Runnable(runnable) });
        }
    }

    visit_file_defs(&Semantics::new(db), file_id, &mut |def| {
        let range = match def {
            Definition::Const(konst) if config.annotate_references => {
                konst.source(db).and_then(|node| name_range(&node, file_id))
            }
            Definition::Trait(trait_) if config.annotate_references || config.annotate_impls => {
                trait_.source(db).and_then(|node| name_range(&node, file_id))
            }
            Definition::Adt(adt) => match adt {
                hir::Adt::Enum(enum_) => {
                    if config.annotate_enum_variant_references {
                        enum_
                            .variants(db)
                            .into_iter()
                            .map(|variant| {
                                variant.source(db).and_then(|node| name_range(&node, file_id))
                            })
                            .filter_map(std::convert::identity)
                            .for_each(|range| {
                                annotations.push(Annotation {
                                    range,
                                    kind: AnnotationKind::HasReferences {
                                        position: FilePosition { file_id, offset: range.start() },
                                        data: None,
                                    },
                                })
                            })
                    }
                    if config.annotate_references || config.annotate_impls {
                        enum_.source(db).and_then(|node| name_range(&node, file_id))
                    } else {
                        None
                    }
                }
                _ => {
                    if config.annotate_references || config.annotate_impls {
                        adt.source(db).and_then(|node| name_range(&node, file_id))
                    } else {
                        None
                    }
                }
            },
            _ => None,
        };

        let (range, offset) = match range {
            Some(range) => (range, range.start()),
            None => return,
        };

        if config.annotate_impls && !matches!(def, Definition::Const(_)) {
            annotations.push(Annotation {
                range,
                kind: AnnotationKind::HasImpls {
                    position: FilePosition { file_id, offset },
                    data: None,
                },
            });
        }
        if config.annotate_references {
            annotations.push(Annotation {
                range,
                kind: AnnotationKind::HasReferences {
                    position: FilePosition { file_id, offset },
                    data: None,
                },
            });
        }

        fn name_range<T: HasName>(node: &InFile<T>, file_id: FileId) -> Option<TextRange> {
            if node.file_id == file_id.into() {
                node.value.name().map(|it| it.syntax().text_range())
            } else {
                // Node is outside the file we are adding annotations to (e.g. macros).
                None
            }
        }
    });

    if config.annotate_method_references {
        annotations.extend(find_all_methods(db, file_id).into_iter().map(
            |FileRange { file_id, range }| Annotation {
                range,
                kind: AnnotationKind::HasReferences {
                    position: FilePosition { file_id, offset: range.start() },
                    data: None,
                },
            },
        ));
    }

    annotations
}

pub(crate) fn resolve_annotation(db: &RootDatabase, mut annotation: Annotation) -> Annotation {
    match &mut annotation.kind {
        AnnotationKind::HasImpls { position, data } => {
            *data = goto_implementation(db, *position).map(|range| range.info);
        }
        AnnotationKind::HasReferences { position, data } => {
            *data = find_all_refs(&Semantics::new(db), *position, None).map(|result| {
                result
                    .into_iter()
                    .flat_map(|res| res.references)
                    .map(|(file_id, access)| {
                        access.into_iter().map(move |(range, _)| FileRange { file_id, range })
                    })
                    .flatten()
                    .collect()
            });
        }
        _ => {}
    };

    annotation
}

fn should_skip_runnable(kind: &RunnableKind, binary_target: bool) -> bool {
    match kind {
        RunnableKind::Bin => !binary_target,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::{fixture, Annotation, AnnotationConfig};

    fn check(ra_fixture: &str, expect: Expect) {
        let (analysis, file_id) = fixture::file(ra_fixture);

        let annotations: Vec<Annotation> = analysis
            .annotations(
                &AnnotationConfig {
                    binary_target: true,
                    annotate_runnables: true,
                    annotate_impls: true,
                    annotate_references: true,
                    annotate_method_references: true,
                    annotate_enum_variant_references: true,
                },
                file_id,
            )
            .unwrap()
            .into_iter()
            .map(|annotation| analysis.resolve_annotation(annotation).unwrap())
            .collect();

        expect.assert_debug_eq(&annotations);
    }

    #[test]
    fn const_annotations() {
        check(
            r#"
const DEMO: i32 = 123;

const UNUSED: i32 = 123;

fn main() {
    let hello = DEMO;
}
            "#,
            expect![[r#"
                [
                    Annotation {
                        range: 53..57,
                        kind: Runnable(
                            Runnable {
                                use_name_in_title: false,
                                nav: NavigationTarget {
                                    file_id: FileId(
                                        0,
                                    ),
                                    full_range: 50..85,
                                    focus_range: 53..57,
                                    name: "main",
                                    kind: Function,
                                },
                                kind: Bin,
                                cfg: None,
                            },
                        ),
                    },
                    Annotation {
                        range: 6..10,
                        kind: HasReferences {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 6,
                            },
                            data: Some(
                                [
                                    FileRange {
                                        file_id: FileId(
                                            0,
                                        ),
                                        range: 78..82,
                                    },
                                ],
                            ),
                        },
                    },
                    Annotation {
                        range: 30..36,
                        kind: HasReferences {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 30,
                            },
                            data: Some(
                                [],
                            ),
                        },
                    },
                    Annotation {
                        range: 53..57,
                        kind: HasReferences {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 53,
                            },
                            data: Some(
                                [],
                            ),
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn struct_references_annotations() {
        check(
            r#"
struct Test;

fn main() {
    let test = Test;
}
            "#,
            expect![[r#"
                [
                    Annotation {
                        range: 17..21,
                        kind: Runnable(
                            Runnable {
                                use_name_in_title: false,
                                nav: NavigationTarget {
                                    file_id: FileId(
                                        0,
                                    ),
                                    full_range: 14..48,
                                    focus_range: 17..21,
                                    name: "main",
                                    kind: Function,
                                },
                                kind: Bin,
                                cfg: None,
                            },
                        ),
                    },
                    Annotation {
                        range: 7..11,
                        kind: HasImpls {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 7,
                            },
                            data: Some(
                                [],
                            ),
                        },
                    },
                    Annotation {
                        range: 7..11,
                        kind: HasReferences {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 7,
                            },
                            data: Some(
                                [
                                    FileRange {
                                        file_id: FileId(
                                            0,
                                        ),
                                        range: 41..45,
                                    },
                                ],
                            ),
                        },
                    },
                    Annotation {
                        range: 17..21,
                        kind: HasReferences {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 17,
                            },
                            data: Some(
                                [],
                            ),
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn struct_and_trait_impls_annotations() {
        check(
            r#"
struct Test;

trait MyCoolTrait {}

impl MyCoolTrait for Test {}

fn main() {
    let test = Test;
}
            "#,
            expect![[r#"
                [
                    Annotation {
                        range: 69..73,
                        kind: Runnable(
                            Runnable {
                                use_name_in_title: false,
                                nav: NavigationTarget {
                                    file_id: FileId(
                                        0,
                                    ),
                                    full_range: 66..100,
                                    focus_range: 69..73,
                                    name: "main",
                                    kind: Function,
                                },
                                kind: Bin,
                                cfg: None,
                            },
                        ),
                    },
                    Annotation {
                        range: 7..11,
                        kind: HasImpls {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 7,
                            },
                            data: Some(
                                [
                                    NavigationTarget {
                                        file_id: FileId(
                                            0,
                                        ),
                                        full_range: 36..64,
                                        focus_range: 57..61,
                                        name: "impl",
                                        kind: Impl,
                                    },
                                ],
                            ),
                        },
                    },
                    Annotation {
                        range: 7..11,
                        kind: HasReferences {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 7,
                            },
                            data: Some(
                                [
                                    FileRange {
                                        file_id: FileId(
                                            0,
                                        ),
                                        range: 57..61,
                                    },
                                    FileRange {
                                        file_id: FileId(
                                            0,
                                        ),
                                        range: 93..97,
                                    },
                                ],
                            ),
                        },
                    },
                    Annotation {
                        range: 20..31,
                        kind: HasImpls {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 20,
                            },
                            data: Some(
                                [
                                    NavigationTarget {
                                        file_id: FileId(
                                            0,
                                        ),
                                        full_range: 36..64,
                                        focus_range: 57..61,
                                        name: "impl",
                                        kind: Impl,
                                    },
                                ],
                            ),
                        },
                    },
                    Annotation {
                        range: 20..31,
                        kind: HasReferences {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 20,
                            },
                            data: Some(
                                [
                                    FileRange {
                                        file_id: FileId(
                                            0,
                                        ),
                                        range: 41..52,
                                    },
                                ],
                            ),
                        },
                    },
                    Annotation {
                        range: 69..73,
                        kind: HasReferences {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 69,
                            },
                            data: Some(
                                [],
                            ),
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn runnable_annotation() {
        check(
            r#"
fn main() {}
            "#,
            expect![[r#"
                [
                    Annotation {
                        range: 3..7,
                        kind: Runnable(
                            Runnable {
                                use_name_in_title: false,
                                nav: NavigationTarget {
                                    file_id: FileId(
                                        0,
                                    ),
                                    full_range: 0..12,
                                    focus_range: 3..7,
                                    name: "main",
                                    kind: Function,
                                },
                                kind: Bin,
                                cfg: None,
                            },
                        ),
                    },
                    Annotation {
                        range: 3..7,
                        kind: HasReferences {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 3,
                            },
                            data: Some(
                                [],
                            ),
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn method_annotations() {
        check(
            r#"
struct Test;

impl Test {
    fn self_by_ref(&self) {}
}

fn main() {
    Test.self_by_ref();
}
            "#,
            expect![[r#"
                [
                    Annotation {
                        range: 61..65,
                        kind: Runnable(
                            Runnable {
                                use_name_in_title: false,
                                nav: NavigationTarget {
                                    file_id: FileId(
                                        0,
                                    ),
                                    full_range: 58..95,
                                    focus_range: 61..65,
                                    name: "main",
                                    kind: Function,
                                },
                                kind: Bin,
                                cfg: None,
                            },
                        ),
                    },
                    Annotation {
                        range: 7..11,
                        kind: HasImpls {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 7,
                            },
                            data: Some(
                                [
                                    NavigationTarget {
                                        file_id: FileId(
                                            0,
                                        ),
                                        full_range: 14..56,
                                        focus_range: 19..23,
                                        name: "impl",
                                        kind: Impl,
                                    },
                                ],
                            ),
                        },
                    },
                    Annotation {
                        range: 7..11,
                        kind: HasReferences {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 7,
                            },
                            data: Some(
                                [
                                    FileRange {
                                        file_id: FileId(
                                            0,
                                        ),
                                        range: 19..23,
                                    },
                                    FileRange {
                                        file_id: FileId(
                                            0,
                                        ),
                                        range: 74..78,
                                    },
                                ],
                            ),
                        },
                    },
                    Annotation {
                        range: 33..44,
                        kind: HasReferences {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 33,
                            },
                            data: Some(
                                [
                                    FileRange {
                                        file_id: FileId(
                                            0,
                                        ),
                                        range: 79..90,
                                    },
                                ],
                            ),
                        },
                    },
                    Annotation {
                        range: 61..65,
                        kind: HasReferences {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 61,
                            },
                            data: Some(
                                [],
                            ),
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_annotations() {
        check(
            r#"
fn main() {}

mod tests {
    #[test]
    fn my_cool_test() {}
}
            "#,
            expect![[r#"
                [
                    Annotation {
                        range: 3..7,
                        kind: Runnable(
                            Runnable {
                                use_name_in_title: false,
                                nav: NavigationTarget {
                                    file_id: FileId(
                                        0,
                                    ),
                                    full_range: 0..12,
                                    focus_range: 3..7,
                                    name: "main",
                                    kind: Function,
                                },
                                kind: Bin,
                                cfg: None,
                            },
                        ),
                    },
                    Annotation {
                        range: 18..23,
                        kind: Runnable(
                            Runnable {
                                use_name_in_title: false,
                                nav: NavigationTarget {
                                    file_id: FileId(
                                        0,
                                    ),
                                    full_range: 14..64,
                                    focus_range: 18..23,
                                    name: "tests",
                                    kind: Module,
                                    description: "mod tests",
                                },
                                kind: TestMod {
                                    path: "tests",
                                },
                                cfg: None,
                            },
                        ),
                    },
                    Annotation {
                        range: 45..57,
                        kind: Runnable(
                            Runnable {
                                use_name_in_title: false,
                                nav: NavigationTarget {
                                    file_id: FileId(
                                        0,
                                    ),
                                    full_range: 30..62,
                                    focus_range: 45..57,
                                    name: "my_cool_test",
                                    kind: Function,
                                },
                                kind: Test {
                                    test_id: Path(
                                        "tests::my_cool_test",
                                    ),
                                    attr: TestAttr {
                                        ignore: false,
                                    },
                                },
                                cfg: None,
                            },
                        ),
                    },
                    Annotation {
                        range: 3..7,
                        kind: HasReferences {
                            position: FilePosition {
                                file_id: FileId(
                                    0,
                                ),
                                offset: 3,
                            },
                            data: Some(
                                [],
                            ),
                        },
                    },
                ]
            "#]],
        );
    }

    #[test]
    fn test_no_annotations_outside_module_tree() {
        check(
            r#"
//- /foo.rs
struct Foo;
//- /lib.rs
// this file comes last since `check` checks the first file only
"#,
            expect![[r#"
                []
            "#]],
        );
    }

    #[test]
    fn test_no_annotations_macro_struct_def() {
        check(
            r#"
//- /lib.rs
macro_rules! m {
    () => {
        struct A {}
    };
}

m!();
"#,
            expect![[r#"
                []
            "#]],
        );
    }
}
