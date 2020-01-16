module AST

data AForm(loc src = |tmp:///|)
  = form(str name, list[AQuestion] questions)
  ; 

data AQuestion(loc src = |tmp:///|)
  = question(str questionString, AId questionId, AType assignmentType, list[AExpr] expresion)
  | \if(AExpr ifCondition, AQuestion ifTrueQuestion, list[AQuestion] ifElseQuestion)
  | block(list[AQuestion] questionBlock)
  ; 

data AExpr(loc src = |tmp:///|)
  = ref(AId id)
  | \int(int intValue)
  | money(real moneyValue)
  | \str(str stringValue)
  | \bool(bool boolean)
  | expr(AExpr expression)
  | not(AExpr expression)
  | mul(AExpr leftExpression, AExpr rightExpression)
  | div(AExpr leftExpression, AExpr rightExpression)
  | \mod(AExpr leftExpression, AExpr rightExpression)
  | add(AExpr leftExpression, AExpr rightExpression)
  | sub(AExpr leftExpression, AExpr rightExpression)
  | gt(AExpr leftExpression, AExpr rightExpression)
  | gte(AExpr leftExpression, AExpr rightExpression)
  | lt(AExpr leftExpression, AExpr rightExpression)
  | lte(AExpr leftExpression, AExpr rightExpression)
  | equal(AExpr leftExpression, AExpr rightExpression)
  | notequal(AExpr leftExpression, AExpr rightExpression)
  | and(AExpr leftExpression, AExpr rightExpression)
  | or(AExpr leftExpression, AExpr rightExpression)
  ;

data AId(loc src = |tmp:///|)
  = id(str name);

data AType(loc src = |tmp:///|)
  = booleanType()
  | integerType()
  | moneyType()
  | stringType()
  ;