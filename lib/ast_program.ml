(* Grammar:
	 HeapRegularCommand ::=  HeapAtomicCommand  |  HeapRegularCommand; HeapRegularCommand  |  HeapRegularCommand + HeapRegularCommand  |  HeapRegularCommand*
	 
	 HeapAtomicCommand ::=  SKIP  |  Identifier = ArithmeticExpression  |  BooleanExpression ?  |  Identifier = alloc()  |  free(Identifier)  |  x = [y]  |  [x] = ArithmeticExpression
	 
	 BooleanExpression ::=  TRUE  |  FALSE  |  !BooleanExpression  |  BooleanExpression && BooleanExpression  |  BooleanExpression || BooleanExpression  |  ArithmeticExpression BooleanComparison ArithmeticExpression
	 
	 BooleanComparison ::=  =  |  !=  |  <  |  <=  |  >  |  >=
	 
	 ArithmeticExpression ::=  INT(n)  |  Identifier  |  ArithmeticExpression BinaryOperator ArithmeticExpression
	 
	 BinaryOperator ::=  +  |  -  |  *  |  /  
*)
module type AnnotationType = sig
	type t
end

module ASTHeapRegularCommands(Annotation: AnnotationType) = struct
	type t = Annotation.t
	type identifier = string [@@deriving show]
	module IdentifierSet = Set.Make(struct type t = identifier let compare = compare end)
	type 'a annotated_node = {node: 'a; annotation: t [@opaque]} [@@deriving show]
	let addAnnotation (node: 'a) (annotation: t) = {node; annotation}
	let getAnnotation (node: 'a annotated_node) = node.annotation
	let removeAnnotation (node: 'a annotated_node) = node.node

	module ArithmeticOperation = struct
		type t =
			| Plus
			| Minus
			| Times
			| Division
		[@@deriving show]
	end

	module BooleanComparison = struct
		type t =
			| Equal
			| NotEqual
			| LessThan
			| LessOrEqual
			| GreaterThan
			| GreaterOrEqual
		[@@deriving show]
	end

	module ArithmeticExpression = struct
		type t_node =
			| Literal of int
			| Variable of identifier
			| BinaryOperation of ArithmeticOperation.t * t * t
		and t = t_node annotated_node
		[@@deriving show]
	end

	module BooleanExpression = struct
		type t_node =
			| True
			| False
			| Not of t
			| And of t * t
			| Or of t * t
			| Comparison of BooleanComparison.t * ArithmeticExpression.t * ArithmeticExpression.t
		and t = t_node annotated_node
		[@@deriving show]
	end

	module HeapAtomicCommand = struct
		type t_node =
			| Skip
			| Assignment of identifier * ArithmeticExpression.t
			| Guard of BooleanExpression.t
			| Allocation of identifier
			| Free of identifier
			| ReadHeap of identifier * identifier
			| WriteHeap of identifier * ArithmeticExpression.t
		and t = t_node annotated_node
		[@@deriving show]

		(* sil-paper Section 6.1 *)
		let modifiedVariables ae =
			match (removeAnnotation ae) with
				| Skip								-> (IdentifierSet.empty)
				| Assignment(id, _)		-> (IdentifierSet.singleton id)
				| Guard(_)						-> (IdentifierSet.empty) 
				| Allocation(id)			-> (IdentifierSet.singleton id)
				| Free(_)							-> (IdentifierSet.empty)
				| ReadHeap(id, _)			-> (IdentifierSet.singleton id)
				| WriteHeap(_, _)			-> (IdentifierSet.empty)
	end

	module HeapRegularCommand = struct
		type t_node =
			| Command of HeapAtomicCommand.t
			| Sequence of t * t
			| NondeterministicChoice of t * t
			| Star of t
		and t = t_node annotated_node
		[@@deriving show]

		(* sil-paper Section 6.1 *)
		let rec modifiedVariables exp = 
			match (removeAnnotation exp) with
				| Command(ac) -> (
						HeapAtomicCommand.modifiedVariables ac
					)
				| NondeterministicChoice(rc1, rc2) -> (
						IdentifierSet.union (modifiedVariables rc1) (modifiedVariables rc2)
					)
				| Sequence(rc1, rc2) -> (
						IdentifierSet.union (modifiedVariables rc1) (modifiedVariables rc2)
					)
				| Star(rc) -> (
						modifiedVariables rc
					)
	end

	let show = HeapRegularCommand.show
	let modifiedVariables = HeapRegularCommand.modifiedVariables
end