module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  = form(str name, list[AQuestion] questions)
  ; 

data AQuestion(loc src = |tmp:///|)
  = question(str questionString, AId questionID, AType answerType, list[AExpr] answerExpressions)
  | \if(AExpr ifCondition, AQuestion ifTrueQuestion, list[AQuestion] elseQuestions)
  | block(list[AQuestion] blockQuestions)
  ; 

data AExpr(loc src = |tmp:///|)
  = ref(AId id)
  | \int(int exprInt)
  | \str(str exprStr)
  | \bool(bool exprBool)
  | expr(AExpr expr)
  | notExpr(AExpr expr)
  | mult(AExpr leftExpr, AExpr rightExpr)
  | div(AExpr leftExpr, AExpr rightExpr)
  | \mod(AExpr leftExpr, AExpr rightExpr)
  | add(AExpr leftExpr, AExpr rightExpr)
  | sub(AExpr leftExpr, AExpr rightExpr)
  | lessThan(AExpr leftExpr, AExpr rightExpr)
  | leq(AExpr leftExpr, AExpr rightExpr)
  | greaterThan(AExpr leftExpr, AExpr rightExpr)
  | greq(AExpr leftExpr, AExpr rightExpr)
  | notEqual(AExpr leftExpr, AExpr rightExpr)
  | equals(AExpr leftExpr, AExpr rightExpr)
  | land(AExpr leftExpr, AExpr rightExpr)
  | lor(AExpr leftExpr, AExpr rightExpr)
  ;

data AType(loc src = |tmp:///|)
  = booleanType()
  | integerType()
  | stringType()
;

data AId(loc src = |tmp:///|)
  = questionID(str name);