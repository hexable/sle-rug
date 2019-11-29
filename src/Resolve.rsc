module Resolve

import AST;

/*
 * Name resolution for QL
 */ 


// modeling declaring occurrences of names
alias Def = rel[str name, loc def];

// modeling use occurrences of names
alias Use = rel[loc use, str name];

alias UseDef = rel[loc use, loc def];

// the reference graph
alias RefGraph = tuple[
  Use uses, 
  Def defs, 
  UseDef useDef
]; 

RefGraph resolve(AForm f) = <us, ds, us o ds>
  when Use us := uses(f), Def ds := defs(f);

// return the use occurences of names in a form
Use uses(AForm f) {
  return {<src, id> | /ref(str id, loc src) := f}; 
}

// return the declarations of names
Def defs(AForm f) {
  return {<questionID.name, questionID.src> | /question(_, AId questionID, _, _) := f}; 
}