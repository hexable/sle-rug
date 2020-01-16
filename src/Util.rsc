module Util

import AST;
import ParseTree;
import Message;
import CST2AST;
import Compile;
import Syntax;
import Resolve;
import IO;
import Check;
import Transform;

start[Form] createForm(loc path) {
	return parse(#start[Form], path);
}

AForm createAForm(loc path) {
	return cst2ast(createForm(path));
}

void createHTML(loc path) {
	 compile(createAForm(path));
}

set[Message] checkForm(loc path) {
	AForm f = createAForm(path);
	return check(f, collect(f), resolve(f).useDef);
}

RefGraph resolveForm(loc path) {
	return resolve(createAForm(path));
}