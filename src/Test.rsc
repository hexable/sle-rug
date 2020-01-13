module Test

import IO;
import Syntax;
import ParseTree;
import CST2AST;
import Check;
import Eval;
import Compile;

void languageTest() {
  concreteSyntaxTree = parse(#start[Form], |project://QL/examples/tax.myql|);
  abstractSyntaxTree = cst2ast(concreteSyntaxTree);
  check(abstractSyntaxTree);
  compile(abstractSyntaxTree);
  
  /*
  TestInput testInput = [];
  testInput += input("hasBoughtHouse",  vbool(true));
  testInput += input("hasMaintLoan",  vbool(true));
  testInput += input("hasSoldHouse",  vbool(true));
  testInput += input("sellingPrice",  vint(1000000));
  testInput += input("privateDebt",  vint(500000));  
  
  return (initialEnv(abstractSyntaxTree) | eval(abstractSyntaxTree, inp, it) | Input inp <- testInput);
  */
}