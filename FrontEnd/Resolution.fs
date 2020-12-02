namespace FrontEnd

open System.Collections
open System.Collections
open System.Collections
open FrontEnd
open FrontEnd
open FrontEnd
open FrontEnd
open FrontEnd
open FrontEnd.AST

module Resolution =

    type FreshId = int
    type Scope = (string * Declaration) list
    type Stack = Scope list
    type ResolutionState = Stack * FreshId

    let makeFresh ((stack, fresh): ResolutionState) (ident: Ident) =
        ((stack, fresh + 1), ident + ":" + string fresh)

    let add ((stack, fresh): ResolutionState) (ident: Ident) (decl: Declaration) =
        match stack with
        | scope :: scopes -> (((ident, decl) :: scope) :: scopes, fresh)
        | [] -> failwithf "ICE: Empty stack when trying to add ident %s" ident

    let push ((stack, fresh): ResolutionState) = ([] :: stack, fresh)

    let pop ((stack, fresh): ResolutionState) =
        match stack with
        | x :: xs -> ((xs, fresh), x)
        | [] -> failwithf "ICE: Empty stack when trying to pop"

    let rec tryFind ident =
        function
        | (x, d) :: xs when x = ident -> Some(d)
        | (x, d) :: xs -> tryFind ident xs
        | [] -> None

    let rec lookup (stack: Stack) (ident: Ident) =
        match stack with
        | scope :: scopes ->
            match tryFind ident scope with
            | Some (x) -> Some(x)
            | None -> lookup scopes ident
        | [] -> None

    let nameOfDecl =
        function
        | VarDecl (ident) -> ident
        | ArrayDecl (ident, _) -> ident
        | RecordDecl (ident, _) -> ident

    let rec resolveLocation ((stack, fresh): ResolutionState) (loc: Location) =
        let span = { From = 0; To = 0 }
        match loc with
        | Variable (ident) ->
            context {
                match lookup stack ident with
                | Some (VarDecl i) -> return ((stack, fresh), Variable(i))
                | Some (ArrayDecl _) -> return! Error(sprintf "Tried using array %s as an integer" ident, span)
                | Some (RecordDecl _) -> return! Error(sprintf "Tried using struct %s as an integer" ident, span)
                | None -> return! Error(sprintf "Used of undeclared integer %s" ident, span)
            }
        | Array (ident, expr) ->
            context {
                match lookup stack ident with
                | Some (VarDecl _) -> return! Error(sprintf "Tried using integer %s as an array" ident, span)
                | Some (ArrayDecl (i, _)) ->
                    let! (s, expr) = resolveArithmeticExpr (stack, fresh) expr
                    return (s, Array(i, expr))
                | Some (RecordDecl _) -> return! Error(sprintf "Tried using struct %s as an arr" ident, span)
                | None -> return! Error(sprintf "Used of undeclared array %s" ident, span)
            }
        | Field (strct, field) ->
            context {
                match lookup stack strct with
                | Some (VarDecl _) -> return! Error(sprintf "Tried accessing field %s on integer %s" field strct, span)
                | Some (ArrayDecl _) -> return! Error(sprintf "Tried accessing field %s on array %s" field strct, span)
                | Some (RecordDecl (name, fields)) ->
                    if List.contains field fields then
                        return ((stack, fresh), Field(name, field))
                    else
                        return! Error(sprintf "Tried accessing field %s but record %s has no such field" field strct, span)
                | None -> return! Error(sprintf "Used of undeclared struct %s" strct, span)
            }

    and resolveArithmeticExpr (s: ResolutionState) =
        function
        | Loc loc ->
            context {
                let! (s, loc) = resolveLocation s loc
                return (s, Loc loc)
            }
        | IntLiteral (x) -> Ok(s, IntLiteral(x))
        | ArithmeticUnary (op, expr) ->
            context {
                let! (s, expr) = resolveArithmeticExpr s expr
                return (s, ArithmeticUnary(op, expr))
            }
        | ArithmeticBinary (lhs, op, rhs) ->
            context {
                let! (s, lhs) = resolveArithmeticExpr s lhs
                let! (s, rhs) = resolveArithmeticExpr s rhs
                return (s, ArithmeticBinary(lhs, op, rhs))
            }

    and resolveBooleanExpr (s: ResolutionState) =
        function
        | BooleanLiteral (lit) -> Ok((s, BooleanLiteral(lit)))
        | BooleanUnary (op, expr) ->
            context {
                let! (s, expr) = resolveBooleanExpr s expr
                return (s, BooleanUnary(op, expr))
            }
        | BooleanBinary (lhs, op, rhs) ->
            context {
                let! (s, lhs) = resolveBooleanExpr s lhs
                let! (s, rhs) = resolveBooleanExpr s rhs
                return (s, BooleanBinary(lhs, op, rhs))
            }
        | Comparison (lhs, op, rhs) ->
            context {
                let! (s, lhs) = resolveArithmeticExpr s lhs
                let! (s, rhs) = resolveArithmeticExpr s rhs
                return (s, Comparison(lhs, op, rhs))
            }

    let rec resolveDecl (s: ResolutionState) =
        function
        | VarDecl ident ->
            context {
                let (s, freshIdent) = makeFresh s ident
                let decl = VarDecl freshIdent
                let s = add s ident decl
                return (s, decl)
            }
        | ArrayDecl (ident, size) ->
            context {
                let (s, freshIdent) = makeFresh s ident
                let decl = ArrayDecl(freshIdent, size)
                let s = add s ident decl
                return (s, decl)
            }
        | RecordDecl (ident, fields) ->
            context {
                let (s, freshIdent) = makeFresh s ident
                let decl = RecordDecl(freshIdent, fields)
                let s = add s ident decl
                return (s, decl)
            }


    let rec resolveStmt (s: ResolutionState) =
        function
        | Allocate decl ->
            context {
                let! (s, decl) = resolveDecl s decl
                return (s, Allocate(decl))
            }
        | Assign (loc, expr) ->
            context {
                let! (s, loc) = resolveLocation s loc
                let! (s, expr) = resolveArithmeticExpr s expr
                return (s, Assign(loc, expr))
            }
        | RecordAssign (ident, fields) ->
            context {
                let! (s, ident, field_names) =
                    context {
                        let span = { From = 0; To = 0 }
                        match lookup (fst s) ident with
                        | Some (VarDecl _) ->
                            return! Error(sprintf "Tried assigning struct literal to integer %s" ident, span)
                        | Some (ArrayDecl _) ->
                            return! Error(sprintf "Tried assigning struct literal to array %s" ident, span)
                        | Some (RecordDecl (name, field_names)) -> return (s, name, field_names)
                        | None -> return! Error(sprintf "Used of undeclared struct %s" ident, span)
                    }

                let fields =
                    List.map (fun ((_, expr), name) -> (name, expr)) (List.zip fields field_names)

                let rec helper =
                    function
                    | (s, (ident, expr) :: xs) ->
                        context {
                            let! (s, expr') = resolveArithmeticExpr s expr
                            let! (s, xs') = helper (s, xs)
                            return (s, (ident, expr') :: xs')
                        }
                    | (s, []) -> Ok((s, []))

                let! (s, fields) = helper (s, fields)

                return (s, RecordAssign(ident, fields))
            }
        | If (cond, thenBlock, elseBlock) ->
            context {
                let! (s, cond) = resolveBooleanExpr s cond
                let! (s, thenBlock) = resolveBlock s thenBlock

                let! (s, elseBlock) =
                    match elseBlock with
                    | Some (b) ->
                        context {
                            let! (s, b) = resolveBlock s b
                            return (s, Some(b))
                        }
                    | None -> Ok(s, None)

                return (s, If(cond, thenBlock, elseBlock))
            }
        | While (cond, loopBlock) ->
            context {
                let! (s, cond) = resolveBooleanExpr s cond
                let! (s, loopBlock) = resolveBlock s loopBlock
                return (s, While(cond, loopBlock))
            }
        | Read (loc) ->
            context {
                let! (s, loc) = resolveLocation s loc
                return (s, Read loc)
            }
        | Write (expr) ->
            context {
                let! (s, expr) = resolveArithmeticExpr s expr
                return (s, Write expr)
            }
        | Free (decl) -> failwithf "ICE: Free(%s) Stmt found during resolution!" (nameOfDecl decl)

    and resolveStmts (s: ResolutionState) =
        function
        | x :: xs ->
            context {
                let! (s, stmt) = resolveStmt s x
                let! (s, stmts) = resolveStmts s xs
                return (s, stmt :: stmts)
            }
        | [] -> Ok((s, []))

    and resolveBlock (s: ResolutionState) (stmts: Block) =
        context {
            let s = push s
            let! (s, stmts) = resolveStmts s stmts
            let (s, scope) = pop s

            let free_stmts =
                List.map (fun (_, decl) -> Free decl) scope

            return (s, stmts @ free_stmts)
        }

    let rec extractDecls (decls: DeclarationInfo) =
        function
        | Allocate (decl) :: stmts -> extractDecls (decls.Add(nameOfDecl decl, decl)) stmts
        | If (_, thenBlock, elseBlock) :: stmts ->
            let decls = extractDecls decls thenBlock

            let decls =
                match elseBlock with
                | Some (b) -> extractDecls decls b
                | None -> decls

            extractDecls decls stmts
        | While (_, loopBlock) :: stmts ->
            let decls = extractDecls decls loopBlock
            extractDecls decls stmts
        | _ :: stmts -> extractDecls decls stmts
        | [] -> decls

    let resolve (root: Block) =
        context {
            let s = push ([], 1)
            let! (_, root) = resolveBlock s root
            let decls = extractDecls Map.empty root
            return (decls, root)
        }
