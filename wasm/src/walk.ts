import {
  Program,
  ASTNode,
  CallStmt,
  CallExpression,
  NumberLiteral,
  Identifier,
} from "./ast";

interface ASTNodeCallback<NodeType extends ASTNode = ASTNode> {
  (node: NodeType): unknown;
}

interface ASTNodeTraversalCallback<NodeType extends ASTNode = ASTNode> {
  (node: NodeType, visitChild: ASTNodeCallback<ASTNode>);
}

type FullVisitor = {
  [NodeTypeName in ASTNode["type"]]: ASTNodeTraversalCallback<
    ASTNode & { type: NodeTypeName }
  >;
};

const noTraverse: ASTNodeTraversalCallback = () => {
  /** nothing to traverse. */
};

const defaultFullVisitor: FullVisitor = {
  CallExpression: (node: CallExpression, v: ASTNodeCallback) => {
    node.args.forEach(v);
    v(node.callee);
  },

  CallStmt: (node: CallStmt, v: ASTNodeCallback) => {
    v(node.expr);
  },

  Identifier: noTraverse,

  NumberLiteral: noTraverse,

  Program: (node: Program, v: ASTNodeCallback) => {
    node.body.forEach(v);
  },
};

export function fullWalk(ast: ASTNode, callback: ASTNodeCallback) {
  function visit(node: ASTNode) {
    if (!defaultFullVisitor[node.type]) {
      throw new Error(`Unexpected node type: ${node.type}`);
    }
    (defaultFullVisitor[node.type] as ASTNodeTraversalCallback)(node, visit);
    callback(node);
  }

  visit(ast);
}
