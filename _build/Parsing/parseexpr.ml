
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TIMES
    | SEMICOLON
    | RPAR
    | PLUS
    | MOD
    | MINUS
    | LPAR
    | INT of (
# 20 "Parsing/parseexpr.mly"
       (int)
# 18 "Parsing/parseexpr.ml"
  )
    | IDENT of (
# 21 "Parsing/parseexpr.mly"
       (string)
# 23 "Parsing/parseexpr.ml"
  )
    | EQUAL
    | EOF
    | DIV
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState4
  | MenhirState0

# 1 "Parsing/parseexpr.mly"
  
  open Expr

# 51 "Parsing/parseexpr.ml"

let rec _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 21 "Parsing/parseexpr.mly"
       (string)
# 67 "Parsing/parseexpr.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (id : (
# 21 "Parsing/parseexpr.mly"
       (string)
# 75 "Parsing/parseexpr.ml"
    )) = _v in
    let _v : (string) = 
# 28 "Parsing/parseexpr.mly"
               ( id )
# 80 "Parsing/parseexpr.ml"
     in
    match _menhir_s with
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (id : (string)) = _v in
        let ((_menhir_stack, _menhir_s, (lhs : (string))), (ao : (string))) = _menhir_stack in
        let _v : (string) = 
# 43 "Parsing/parseexpr.mly"
                                                           ( lhs^" "^ao^" "^id )
# 91 "Parsing/parseexpr.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (a : (string)) = _v in
        let _v : (string) = 
# 40 "Parsing/parseexpr.mly"
                   ( a )
# 99 "Parsing/parseexpr.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ae : (string)) = _v in
        let _v : (string) = 
# 33 "Parsing/parseexpr.mly"
                            ( ae )
# 107 "Parsing/parseexpr.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (string))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 23 "Parsing/parseexpr.mly"
       (string)
# 122 "Parsing/parseexpr.ml"
            ) = 
# 56 "Parsing/parseexpr.mly"
                     (e)
# 126 "Parsing/parseexpr.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 23 "Parsing/parseexpr.mly"
       (string)
# 133 "Parsing/parseexpr.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (id : (string)) = _v in
        let _v : (string) = 
# 50 "Parsing/parseexpr.mly"
                    ( id )
# 149 "Parsing/parseexpr.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (en : (string)) = _v in
        let _v : (string) = 
# 46 "Parsing/parseexpr.mly"
                        ( en )
# 157 "Parsing/parseexpr.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (string) = 
# 37 "Parsing/parseexpr.mly"
            ( "=" )
# 172 "Parsing/parseexpr.ml"
             in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and compilationUnit : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 23 "Parsing/parseexpr.mly"
       (string)
# 207 "Parsing/parseexpr.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 58 "Parsing/parseexpr.mly"
  
# 230 "Parsing/parseexpr.ml"

# 219 "/usr/share/menhir/standard.mly"
  


# 236 "Parsing/parseexpr.ml"
