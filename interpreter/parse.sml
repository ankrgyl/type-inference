structure Parse  =
struct
  exception Parse of string    

  structure ExpLrVals = ExpLrValsFun(structure Token = LrParser.Token)
  structure Lex = Exp_LexFun (structure Tokens = ExpLrVals.Tokens)
  structure ExpP = Join (structure ParserData = ExpLrVals.ParserData
                         structure Lex = Lex
                         structure LrParser = LrParser)

  (* Turn a stream into a function that returns a new value
   * on each application. This is then given to lex for lex
   * to use convert into a different sort of stream. Sigh. *)
  fun streamreader s =
      let val stream = ref s
          fun reader n =
              case Stream.force (!stream) of
                  Stream.Nil => ""
                | Stream.Cons (x, s) => (stream := s; str x)
      in reader end

  fun error (s, pos, pos') = (print (s ^ "\n"); raise Parse (s ^ "\n"))

  fun is_eof stream =
      let val EOF_token = ExpLrVals.Tokens.EOF (0, 0)
          val (token, _) = LrParser.Stream.get stream
      in
          LrParser.Token.sameToken (token, EOF_token)
      end

  (* Produces a stream of parsed declarations from an input char stream.
   * If an exception gets raised, calls handler with it. Handler should
   * either throw an exception of its own or return a stream that parse
   * will then force and return (this is to allow parse to be restarted
   * when being used for command line operation.
   * The reason for this stream based design is twofold:
   *  1) I thought it would be pretty
   *  2) It is required to make parsing interact nicely with readKeybd.
   *)
  fun parse handler stream =
      let 
          val lexer = ExpP.makeLexer (streamreader stream)
          fun parse' lexer =
              if is_eof lexer then Stream.Nil
              else
              let
                  val (res, lexer') = ExpP.parse (1, lexer, error, ())
                  (* parse is complete -- skip the next token, either ; or eof *)
                  val (_, lexer'') = LrParser.Stream.get lexer'
                  val stream = foldl Stream.cons (parse'' lexer'') res
              in Stream.force stream end
              handle e => (Stream.force (handler e))
          and parse'' lexer = Stream.delay (fn () => parse' lexer)
      in
          parse'' lexer
      end

end
