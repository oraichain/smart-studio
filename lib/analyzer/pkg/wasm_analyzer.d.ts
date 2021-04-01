/* tslint:disable */
/**
*/
export function start(): void;
/**
*/
export class WorldState {
  free(): void;
/**
* @returns {WorldState} 
*/
  constructor();
/**
* @param {string} code 
* @returns {any} 
*/
  update(code: string): any;
/**
* @param {number} line_number 
* @param {number} column 
* @returns {any} 
*/
  completions(line_number: number, column: number): any;
/**
* @param {number} line_number 
* @param {number} column 
* @returns {any} 
*/
  hover(line_number: number, column: number): any;
/**
* @returns {any} 
*/
  code_lenses(): any;
/**
* @param {number} line_number 
* @param {number} column 
* @param {boolean} include_declaration 
* @returns {any} 
*/
  references(line_number: number, column: number, include_declaration: boolean): any;
/**
* @param {number} line_number 
* @param {number} column 
* @returns {any} 
*/
  prepare_rename(line_number: number, column: number): any;
/**
* @param {number} line_number 
* @param {number} column 
* @param {string} new_name 
* @returns {any} 
*/
  rename(line_number: number, column: number, new_name: string): any;
/**
* @param {number} line_number 
* @param {number} column 
* @returns {any} 
*/
  signature_help(line_number: number, column: number): any;
/**
* @param {number} line_number 
* @param {number} column 
* @returns {any} 
*/
  definition(line_number: number, column: number): any;
/**
* @param {number} line_number 
* @param {number} column 
* @returns {any} 
*/
  type_definition(line_number: number, column: number): any;
/**
* @returns {any} 
*/
  document_symbols(): any;
/**
* @param {number} line_number 
* @param {number} column 
* @param {string} ch 
* @returns {any} 
*/
  type_formatting(line_number: number, column: number, ch: string): any;
/**
* @returns {any} 
*/
  folding_ranges(): any;
/**
* @param {number} line_number 
* @param {number} column 
* @returns {any} 
*/
  goto_implementation(line_number: number, column: number): any;
}
