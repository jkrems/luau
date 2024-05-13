import { Range } from "./luau.pegjs";

export interface Identifier {
  type: "Identifier";
  loc: Range;
  name: string;
}

export interface NumberLiteral {
  type: "NumberLiteral";
  loc: Range;
  value: number;
}

export interface CallExpression {
  type: "CallExpression";
  loc: Range;
  callee: ASTNode;
  args: ASTNode[];
}

export interface CallStmt {
  type: "CallStmt";
  expr: ASTNode;
  loc: Range;
}

export interface Program {
  type: "Program";
  body: ASTNode[];
  loc: Range;
}

export type ASTNode =
  | Identifier
  | NumberLiteral
  | CallExpression
  | CallStmt
  | Program;
