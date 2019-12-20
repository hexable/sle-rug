module Test

import Syntax;
import ParseTree;
import CST2AST;
import Check;

set[Message] languageTest() {
  concreteSyntaxTree = parse(#start[Form], |project://QL/examples/tests.myql|);
  abstractSyntaxTree = cst2ast(concreteSyntaxTree);
  return check(abstractSyntaxTree);
}