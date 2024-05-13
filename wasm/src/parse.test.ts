import { expect, test } from "vitest";

import { parse } from "./parse";
import { fullWalk } from "./walk";

interface ASTAssertion {
  type: string;
  startColumn: number;
  length: number;
}

function expectValidAST(spec: string) {
  const [header, ...sourceOrAssertion] = spec.split("\n");
  test(header.replace(/^\/\/ /, ""), () => {
    const codeLines: string[] = [];
    const expectedsByLine = new Map<number, string[]>();
    const actualsByLine = new Map<number, string[]>();
    let expecteds: string[] = [];
    for (const line of sourceOrAssertion) {
      const match = line.match(/^([ ]*)([\~]+)\: (.+)$/);
      if (match) {
        expecteds.push(line);
      } else {
        codeLines.push(line);
        expecteds = [];
        expectedsByLine.set(codeLines.length, expecteds);
        actualsByLine.set(codeLines.length, []);
      }
    }
    const code = codeLines.join("\n");
    const ast = parse(code);

    fullWalk(ast, (n) => {
      expect(n.type, "Invalid node, missing type").toBeTypeOf("string");
      expect(n.loc, `Invalid ${n.type} node, missing location`).toBeTruthy();
      expect(
        n.loc.start,
        `Invalid ${n.type} node, missing start location`
      ).toBeTruthy();
      expect(
        n.loc.end,
        `Invalid ${n.type} node, missing end location`
      ).toBeTruthy();
      if (n.loc.start.line !== n.loc.end.line) {
        // Ignore nodes that span more than one line.
        return;
      }

      actualsByLine
        .get(n.loc.start.line)!
        .push(
          `${" ".repeat(n.loc.start.column - 1)}${"~".repeat(
            n.loc.end.column - n.loc.start.column
          )}: ${n.type}`
        );
    });

    for (const [lineNumber, expected] of expectedsByLine) {
      const actual = actualsByLine.get(lineNumber)!;
      if (!actual.length && !expected.length) continue;
      expect(actual, `${lineNumber}: ${codeLines[lineNumber - 1]}`).toEqual(
        expected
      );
    }
  });
}

expectValidAST(`// Function call
print(10);
      ~~: NumberLiteral
~~~~~: Identifier
~~~~~~~~~: CallExpression
~~~~~~~~~: CallStmt
`);
