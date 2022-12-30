use std::sync::Arc;

use ide::{
    AdjustmentHints, AnalysisHost, CallableSnippets, Change, ClosureReturnTypeHints,
    CompletionConfig, DiagnosticsConfig, ExprFillDefaultMode, FileId, FilePosition, FileRange,
    HighlightConfig, HoverConfig, HoverDocFormat, Indel, InlayHintsConfig, InlayKind, InlayTooltip,
    LifetimeElisionHints, TextRange, TextSize,
};
use ide_db::{
    imports::insert_use::{ImportGranularity, InsertUseConfig, PrefixKind},
    search::SearchScope,
    FxHashSet, SnippetCap,
};

use crate::extractor;
use crate::return_types::*;
use crate::to_proto::*;

// #[wasm_bindgen]
pub struct LocalState {
    host: AnalysisHost,
}

impl Default for LocalState {
    fn default() -> Self {
        let host = AnalysisHost::default();
        Self { host }
    }
}

const INSERT_USER_CONFIG: InsertUseConfig = InsertUseConfig {
    granularity: ImportGranularity::Crate,
    enforce_granularity: true,
    prefix_kind: PrefixKind::Plain,
    group: true,
    skip_glob_imports: true,
};

impl LocalState {
    pub fn apply_change(&mut self, change: Change) {
        self.host.apply_change(change);
    }

    pub fn load(&mut self, json: Vec<u8>) {
        let json = String::from_utf8_lossy(&json);
        let change = extractor::load_change_from_json(&json);

        // apply change
        self.apply_change(change);
    }

    pub fn update(&mut self, file_ind: u32, code: String, with_highlight: bool) -> UpdateResult {
        let file_id = FileId(file_ind);

        let mut change = Change::new();
        change.change_file(file_id, Some(Arc::new(code)));
        self.apply_change(change);

        let analysis = self.host.analysis();
        let line_index = analysis.file_line_index(file_id).unwrap();

        const HIGHLIGHT_CONFIG: HighlightConfig = HighlightConfig {
            strings: true,
            punctuation: true,
            specialize_punctuation: true,
            specialize_operator: true,
            operator: true,
            inject_doc_comment: true,
            macro_bang: true,
            syntactic_name_ref_highlighting: true,
        };

        let highlights = if with_highlight {
            analysis
                .highlight(HIGHLIGHT_CONFIG, file_id)
                .unwrap()
                .into_iter()
                .map(|hl| Highlight {
                    fileId: file_ind,
                    tag: Some(hl.highlight.tag.to_string()),
                    range: text_range(&hl.range, &line_index),
                })
                .collect()
        } else {
            vec![]
        };

        let mut config = DiagnosticsConfig {
            proc_macros_enabled: true,
            proc_attr_macros_enabled: true,
            disable_experimental: true,
            disabled: FxHashSet::default(),
            expr_fill_default: ExprFillDefaultMode::Default,
            insert_use: INSERT_USER_CONFIG,
            prefer_no_std: false,
        };
        config.disabled.insert("unresolved-proc-macro".to_string());
        config.disabled.insert("unresolved-macro-call".to_string());

        let diagnostics = analysis
            .diagnostics(&config, ide::AssistResolveStrategy::None, file_id)
            .unwrap()
            .into_iter()
            .map(|d| {
                let Range { startLineNumber, startColumn, endLineNumber, endColumn } =
                    text_range(&d.range, &line_index);
                Diagnostic {
                    message: d.message,
                    severity: severity(d.severity),
                    startLineNumber,
                    startColumn,
                    endLineNumber,
                    endColumn,
                }
            })
            .collect();
        UpdateResult { diagnostics, highlights }
    }

    pub fn inlay_hints(&self, file_ind: u32) -> Vec<InlayHint> {
        let file_id = FileId(file_ind);

        const INLAY_HINTS_CONFIG: InlayHintsConfig = InlayHintsConfig {
            type_hints: true,
            parameter_hints: true,
            chaining_hints: true,
            hide_named_constructor_hints: false,
            max_length: Some(25),
            location_links: true,
            render_colons: true,
            adjustment_hints: AdjustmentHints::Always,
            adjustment_hints_hide_outside_unsafe: true,
            closure_return_type_hints: ClosureReturnTypeHints::Always,
            binding_mode_hints: true,
            lifetime_elision_hints: LifetimeElisionHints::Always,
            param_names_for_lifetime_elision_hints: true,
            hide_closure_initialization_hints: false,
            closing_brace_hints_min_lines: None,
        };

        let analysis = self.host.analysis();
        let line_index = analysis.file_line_index(file_id).unwrap();
        analysis
            .inlay_hints(&INLAY_HINTS_CONFIG, file_id, None)
            .unwrap()
            .into_iter()
            .map(|ih| InlayHint {
                label: Some(ih.label.to_string()),
                tooltip: ih.tooltip.map(|tooltip| match tooltip {
                    InlayTooltip::String(str) => str,
                    _ => String::new(),
                }),
                hint_type: match ih.kind {
                    InlayKind::ParameterHint => InlayHintType::Parameter,
                    _ => InlayHintType::Type,
                },

                range: text_range(&ih.range, &line_index),
            })
            .collect()
    }

    pub fn completions(&self, file_ind: u32, line_number: u32, column: u32) -> Vec<CompletionItem> {
        let file_id = FileId(file_ind);

        const COMPLETION_CONFIG: CompletionConfig = CompletionConfig {
            enable_postfix_completions: true,
            enable_imports_on_the_fly: true,
            enable_self_on_the_fly: true,
            enable_private_editable: true,
            callable: Some(CallableSnippets::FillArguments),
            snippet_cap: SnippetCap::new(true),
            snippets: Vec::new(),
            prefer_no_std: false,
            insert_use: INSERT_USER_CONFIG,
        };

        let analysis = self.host.analysis();
        let line_index = analysis.file_line_index(file_id).unwrap();

        let pos = file_position(line_number, column, &line_index, file_id);
        let res = match analysis.completions(&COMPLETION_CONFIG, pos, None).unwrap() {
            Some(items) => items,
            None => return vec![],
        };

        res.into_iter().map(|item| completion_item(item, &line_index)).collect()
    }

    pub fn hover(&self, file_ind: u32, line_number: u32, column: u32) -> Option<Hover> {
        let file_id = FileId(file_ind);
        let analysis = self.host.analysis();
        let line_index = analysis.file_line_index(file_id).unwrap();

        const HOVER_CONFIG: HoverConfig = HoverConfig {
            links_in_hover: true,
            documentation: Some(HoverDocFormat::Markdown),
            keywords: true,
        };

        let FilePosition { file_id, offset } =
            file_position(line_number, column, &line_index, file_id);
        let range = FileRange {
            file_id,
            range: TextRange::new(TextSize::from(offset), TextSize::from(offset)),
        };

        let info = match analysis.hover(&HOVER_CONFIG, range).unwrap() {
            Some(info) => info,
            _ => return None,
        };

        let value = info.info.markup.to_string();
        Some(Hover {
            contents: vec![MarkdownString { value }],
            range: text_range(&info.range, &line_index),
        })
    }

    pub fn code_lenses(&self, file_ind: u32) -> Vec<CodeLensSymbol> {
        let file_id = FileId(file_ind);
        let analysis = self.host.analysis();
        let line_index = analysis.file_line_index(file_id).unwrap();

        analysis
            .file_structure(file_id)
            .unwrap()
            .into_iter()
            .filter(|it| match it.kind {
                ide::StructureNodeKind::SymbolKind(it) => matches!(
                    it,
                    ide_db::SymbolKind::Trait
                        | ide_db::SymbolKind::Struct
                        | ide_db::SymbolKind::Enum
                ),
                ide::StructureNodeKind::Region => true,
            })
            .filter_map(|it| {
                let position = FilePosition { file_id, offset: it.node_range.start() };
                let nav_info = analysis.goto_implementation(position).unwrap()?;

                let title = if nav_info.info.len() == 1 {
                    "1 implementation".into()
                } else {
                    format!("{} implementations", nav_info.info.len())
                };

                let positions = nav_info
                    .info
                    .iter()
                    .map(|target| target.focus_or_full_range())
                    .map(|range| text_range(&range, &line_index))
                    .collect();

                Some(CodeLensSymbol {
                    range: text_range(&it.node_range, &line_index),
                    command: Some(Command {
                        id: "editor.action.showReferences".into(),
                        title,
                        positions,
                    }),
                })
            })
            .collect()
    }

    pub fn references(
        &self,
        file_ind: u32,
        line_number: u32,
        column: u32,
        include_declaration: bool,
    ) -> Vec<Highlight> {
        let file_id = FileId(file_ind);
        let analysis = self.host.analysis();
        let line_index = analysis.file_line_index(file_id).unwrap();

        let pos = file_position(line_number, column, &line_index, file_id);
        let search_scope = Some(SearchScope::single_file(file_id));
        let ref_results = match analysis.find_all_refs(pos, search_scope) {
            Ok(Some(info)) => info,
            _ => return vec![],
        };

        let mut res = vec![];
        for ref_result in ref_results {
            if include_declaration {
                if let Some(d) = ref_result.declaration {
                    let r = d.nav.focus_range.unwrap_or(d.nav.full_range);
                    res.push(Highlight {
                        fileId: d.nav.file_id.0,
                        tag: None,
                        range: text_range(&r, &line_index),
                    });
                }
            }
            ref_result.references.iter().for_each(|(id, ranges)| {
                // handle multiple files
                for (r, _) in ranges {
                    res.push(Highlight {
                        fileId: id.0,
                        tag: None,
                        range: text_range(&*r, &line_index),
                    });
                }
            });
        }

        res
    }

    pub fn prepare_rename(
        &self,
        file_ind: u32,
        line_number: u32,
        column: u32,
    ) -> Option<RenameLocation> {
        let file_id = FileId(file_ind);
        let analysis = self.host.analysis();
        let line_index = analysis.file_line_index(file_id).unwrap();

        let pos = file_position(line_number, column, &line_index, file_id);
        let range_info = match analysis.prepare_rename(pos).unwrap() {
            Ok(refs) => refs,
            _ => return None,
        };

        let range = text_range(&range_info.range, &line_index);
        let file_text = analysis.file_text(file_id).unwrap();
        let text = file_text[range_info.range].to_owned();

        Some(RenameLocation { range, text })
    }

    pub fn rename(
        &self,
        file_ind: u32,
        line_number: u32,
        column: u32,
        new_name: &str,
    ) -> Vec<TextEdit> {
        let file_id = FileId(file_ind);
        let analysis = self.host.analysis();
        let line_index = analysis.file_line_index(file_id).unwrap();

        let pos = file_position(line_number, column, &line_index, file_id);
        let change = match analysis.rename(pos, new_name).unwrap() {
            Ok(change) => change,
            Err(_) => return vec![],
        };

        change
            .source_file_edits
            .iter()
            .flat_map(|(_, edit)| edit.iter())
            .map(|atom: &Indel| text_edit(atom, &line_index))
            .collect()
    }

    pub fn signature_help(
        &self,
        file_ind: u32,
        line_number: u32,
        column: u32,
    ) -> Option<SignatureHelp> {
        let file_id = FileId(file_ind);

        let analysis = self.host.analysis();
        let line_index = analysis.file_line_index(file_id).unwrap();

        let pos = file_position(line_number, column, &line_index, file_id);
        let signature_help = match analysis.signature_help(pos) {
            Ok(Some(signature_help)) => signature_help,
            _ => return None,
        };

        let active_parameter = signature_help.active_parameter;
        let sig_info = signature_information(signature_help);

        Some(SignatureHelp {
            signatures: [sig_info],
            activeSignature: 0,
            activeParameter: active_parameter,
        })
    }

    pub fn definition(&self, file_ind: u32, line_number: u32, column: u32) -> Vec<LocationLink> {
        let file_id = FileId(file_ind);
        let analysis = self.host.analysis();
        let line_index = analysis.file_line_index(file_id).unwrap();

        let pos = file_position(line_number, column, &line_index, file_id);
        let nav_info = match analysis.goto_definition(pos) {
            Ok(Some(nav_info)) => nav_info,
            _ => return vec![],
        };

        location_links(nav_info, &line_index, &analysis)
    }

    pub fn type_definition(
        &self,
        file_ind: u32,
        line_number: u32,
        column: u32,
    ) -> Vec<LocationLink> {
        let file_id = FileId(file_ind);
        let analysis = self.host.analysis();
        let line_index = analysis.file_line_index(file_id).unwrap();

        let pos = file_position(line_number, column, &line_index, file_id);
        let nav_info = match analysis.goto_type_definition(pos) {
            Ok(Some(nav_info)) => nav_info,
            _ => return vec![],
        };

        location_links(nav_info, &line_index, &analysis)
    }

    pub fn document_symbols(&self, file_ind: u32) -> Vec<DocumentSymbol> {
        let file_id = FileId(file_ind);
        let analysis = self.host.analysis();
        let line_index = analysis.file_line_index(file_id).unwrap();

        let struct_nodes = match analysis.file_structure(file_id) {
            Ok(struct_nodes) => struct_nodes,
            _ => return vec![],
        };
        let mut parents: Vec<(DocumentSymbol, Option<usize>)> = Vec::new();

        for symbol in struct_nodes {
            let doc_symbol = DocumentSymbol {
                name: symbol.label.clone(),
                detail: symbol.detail.unwrap_or(symbol.label),
                kind: symbol_kind(symbol.kind),
                range: text_range(&symbol.node_range, &line_index),
                children: None,
                tags: [if symbol.deprecated { SymbolTag::Deprecated } else { SymbolTag::None }],
                containerName: None,
                selectionRange: text_range(&symbol.navigation_range, &line_index),
            };
            parents.push((doc_symbol, symbol.parent));
        }
        let mut res = Vec::new();
        while let Some((node, parent)) = parents.pop() {
            match parent {
                None => res.push(node),
                Some(i) => {
                    let children = &mut parents[i].0.children;
                    if children.is_none() {
                        *children = Some(Vec::new());
                    }
                    children.as_mut().unwrap().push(node);
                }
            }
        }

        res
    }

    pub fn type_formatting(
        &self,
        file_ind: u32,
        line_number: u32,
        column: u32,
        ch: char,
    ) -> Vec<TextEdit> {
        let file_id = FileId(file_ind);
        let analysis = self.host.analysis();
        let line_index = analysis.file_line_index(file_id).unwrap();

        let mut pos = file_position(line_number, column, &line_index, file_id);
        pos.offset -= TextSize::of('.');

        let edit = analysis.on_char_typed(pos, ch, false);

        let (_file, edit) = match edit {
            Ok(Some(it)) => it.source_file_edits.into_iter().next().unwrap(),
            _ => return vec![],
        };

        text_edits(edit, &line_index)
    }

    pub fn folding_ranges(&self, file_ind: u32) -> Vec<FoldingRange> {
        let file_id = FileId(file_ind);

        let analysis = self.host.analysis();
        let line_index = analysis.file_line_index(file_id).unwrap();
        if let Ok(folds) = analysis.folding_ranges(file_id) {
            return folds.into_iter().map(|fold| folding_range(fold, &line_index)).collect();
        }
        vec![]
    }

    pub fn goto_implementation(
        &self,
        file_ind: u32,
        line_number: u32,
        column: u32,
    ) -> Vec<LocationLink> {
        let file_id = FileId(file_ind);
        let analysis = self.host.analysis();
        let line_index = analysis.file_line_index(file_id).unwrap();

        let pos = file_position(line_number, column, &line_index, file_id);
        let nav_info = match analysis.goto_implementation(pos) {
            Ok(Some(it)) => it,
            _ => return vec![],
        };
        location_links(nav_info, &line_index, &analysis)
    }
}

fn file_position(
    line_number: u32,
    column: u32,
    line_index: &ide::LineIndex,
    file_id: ide::FileId,
) -> ide::FilePosition {
    let line_col = ide::LineCol { line: line_number - 1, col: column - 1 };
    let offset = line_index.offset(line_col).unwrap();
    ide::FilePosition { file_id, offset }
}
