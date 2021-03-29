/* Any copyright is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/ */

import registerLanguages from '../../src/utils/registerLanguages';

declare var monaco: { languages };

function mockFetch() {
  (global as any).fetch = jest.fn().mockImplementation((arg) => {
    return { text: () => 'fetched: ' + arg };
  });
  return () => ((global as any).fetch = undefined);
}

describe('Tests for registerLanguages', () => {
  it('should correctly configure TypeScript/JavaScript', async () => {
    const restoreFetch = mockFetch();
    const addExtraLib = jest.spyOn(monaco.languages.typescript.typescriptDefaults, 'addExtraLib');
    const setTsCompilerOptions = jest.spyOn(monaco.languages.typescript.typescriptDefaults, 'setCompilerOptions');
    const setJsCompilerOptions = jest.spyOn(monaco.languages.typescript.javascriptDefaults, 'setCompilerOptions');
    await registerLanguages();
    expect(addExtraLib).toHaveBeenCalledWith('fetched: lib/lib.es6.d.ts');
    expect(addExtraLib).toHaveBeenCalledWith('fetched: lib/fiddle.d.ts');
    expect(setTsCompilerOptions).toHaveBeenCalledWith({ noLib: true, allowNonTsExtensions: true });
    expect(setJsCompilerOptions).toHaveBeenCalledWith({ noLib: true, allowNonTsExtensions: true });
    addExtraLib.mockRestore();
    setTsCompilerOptions.mockRestore();
    setJsCompilerOptions.mockRestore();
    restoreFetch();
  });
});
