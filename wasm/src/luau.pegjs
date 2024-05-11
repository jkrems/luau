Program
  = __ body:Chunk __
  { return {type: "Program", body, loc: location()}; }

EOF
  = !.

Chunk
  = stmts:(
    head:(s:Stat __ ";"? { return s; })
    tail:(__ s:Stat __ ";"? { return s; })*
    {
      return [head, ...tail];
    }
  )? last:(__ s:LastStat __ ";"? {return s;})?
  {
    stmts ??= [];
    return last ? [...stmts, last] : stmts;
  }

Block = Chunk

Stat
  = expr:FunctionCall !(__ "=") { return {type: 'CallStmt', expr}; }
  / vars:VarList __ "=" __ exprs:ExpList {
      return {type: 'VarDecl', local: false, vars, exprs, loc: location()};
    }
  / "do" body:Block "end" { return {type: 'BlockStmt', body}; }
  / "while" cond:Exp "do" body:Block "end"
  / "repeat" body:Block "until" cond:Exp
  / "if" Exp "then" Block ("elseif" Exp "then" Block)* ("else" Block)? "end"
  / "function" _ name:FuncName __ func:FuncBody {
    return {type: 'FuncDecl', local: false, name, ...func, loc: location()};
  }
  / "local" _ "function" __ name:NAME __ func:FuncBody {
    return {type: 'FuncDecl', local: true, name, ...func, loc: location()};
  }
  / "local" __ vars:BindingList exprs:(__ "=" __ exprs:ExpList { return exprs; })? {
    return {type: 'VarDecl', local: true, vars, exprs, loc: location()};
  }
  / TypeDecl

LastStat
  = "return" __ expr:ExpList? {
    return {type: 'Return', expr, loc: location()};
  }
  / "break" {
    return {type: 'Break', loc: location()};
  }

TypeDecl
  = exported:("export" __)? "type" __ name:NAME __ args:TypeArgs? __ "=" __ def:Type {
    return {
      type: 'TypeDecl',
      name,
      args: args ?? [],
      exported: !!exported,
      def,
      loc: location(),
    };
  }

TypeArgs
  = "<" GenericTypeListWithDefaults ">"

FuncName
  = base:NAME props:("." n:NAME { return n; })* method:(":" n:NAME { return n})? {
    return {base, props, method, loc: location()};
  }

VarList
  = head:Var tail:( __ "," __ v:Var { return v; })* {
    // TODO: Verify that the target isn't a function call.
    return [head, ...tail];
  }

Var
  = access:PropertyAccessOrFunctionCall { return access; }
  / name:NAME { return {type: 'Identifier', name, loc: location()}; }

BindingList
  = head:Binding tail:( __ "," __ n:Binding { return n; })* {
    return [head, ...tail];
  }

Binding
  = name:NAME type:TypeAnnotation? {
    return {name, type, loc: location()};
  }

TypeAnnotation
  = __ ":" __ t:Type { return t; }

ExpList
  = head:Exp tail:( __ "," __ e:Exp { return e; })* {
    return [head, ...tail];
  }

Exp
  = exp:SimpleExp bins:(__ op:BinOp __ right:Exp {return {op, right}; })* {
    let initialExp = exp;
    if (bins.length) {
      return bins.reduce((left, {op, right}) => {
        return {
          type: 'BinaryExp',
          op,
          left,
          right,
          loc: location(),
        };
      }, exp);
    }
    return initialExp;
  }

SimpleExp
  = "nil"
  / value:("false" / "true") {
    return {type: 'Boolean', value: value === 'true', loc: location()};
  }
  / value:Number {
    return {type: 'Number', value, loc: location()};
  }
  / value:String {
    return {type: 'String', value, loc: location()};
  }
  / "..." { return {type: 'Rest', loc: location()}; }
  / Function
  / FunctionCall
  / Var
  / "(" Exp ")"
  / TableConstructor
  / UnOp __ Exp

PropertyAccessOrFunctionCall
  = head:PropertyAccessStart tail:CallOrRead* {
    return tail.reduce((n, cor) => {
      switch (cor.type) {
        case 'PropertyRead': {
          return {
            type: 'PropertyAccess',
            object: n,
            key: cor.key,
            computed: cor.computed,
            loc: location(),
          };
        }

        default: {
          console.error(cor);
          throw new Error(`Unsupported CallOrRead: ${cor.type}`);
        }
      }
    }, head);
  }

PropertyRead
  = "[" key:Exp "]" { return {type: 'PropertyRead', key, computed: true, loc: location()}; }
  / "." key:NAME { return {type: 'PropertyRead', key, computed: false, loc: location()}; }

CallOrRead
  = Call
  / PropertyRead

PropertyAccessStart
  = name:NAME { return {type: 'Identifier', name, loc: location()}; }
  / "(" expr:Exp ")" { return expr; }

FunctionCall
  = initialCallee:(
      start:PropertyAccessStart props:PropertyRead* {
        return props.reduce((n, prop) => {
          return {
            type: 'PropertyAccess',
            object: n,
            key: prop.key,
            computed: prop.computed,
            loc: location(),
          };
        }, start);
      }
    ) calls:(
      head:Call tail:Call*
      {
        return [head, ...tail];
      }
    ) {
      // a(b)(c)(d)
      let callee = initialCallee;

      return calls.map((call) => {
        const node = {
          type: 'CallExpression',
          callee,
          method: call.method,
          args: call.args,
          loc: {
            start: callee.start,
            end: call.end,
          },
        };
        callee = node;
        return node;
      })[0];
    }

Call
  = ":" method:NAME args:Args { return {method, args, loc: location()}; }
  / args:Args { return {args, loc: location()}; }

Args
  = "(" __ expr:ExpList? __ ")" { return expr ? expr : []; }
  / TableConstructor
  / String

Function
  = "function" func:FuncBody { return {name: null, ...func, loc: location()}; }

FuncBody
  = "(" __ paramList:ParList? __ ")" returnType:ReturnTypeAnnotation? __ body:Block __ "end" {
    return {...paramList, returnType, body, loc: location()};
  }

ReturnTypeAnnotation
  = __ ":" __ t:ReturnType { return t; }

ParList
  = params:BindingList rest:( __ "," __ "...")? {
    return {params, rest: !!rest, loc: location()};
  }
  / "..." {
    return {params: [], rest: true, loc: location()};
  }

TableConstructor
  = "{" __ fields:FieldList? __ "}" {
    return {
      type: 'TableLiteral',
      fields: fields ?? [],
      loc: location(),
    };
  }

FieldList
  = head:Field tail:( __ FieldSep __ f:Field { return f; })* ( __ FieldSep)? {
    return [head, ...tail];
  }

Field
  = "[" key:Exp "]" __ "=" __ value:Exp {
    return {type: 'Field', key, value, computed: true, loc: location()};
  }
  / key:NAME __ "=" __ value:Exp {
    return {type: 'Field', key, value, loc: location()};
  }
  / value:Exp {
    return {type: 'Field', key: null, value, loc: location()};
  }

FieldSep
  = ","
  / ";"

BinOp
  = "+" / "-" / "*" / "/" / "^" / "%" / ".." / "<" / "<=" / ">" / ">=" / "=="
  / "~=" / "and" / "or"

UnOp
  = "-" / "not" / "#"

SimpleType
  = "nil"
  / SingletonType
  / base:NAME props:( "." n:NAME { return n; })* { // TODO: TypeParams
    return {
      type: 'TypeRef',
      base,
      props,
      loc: location(),
    };
  }
  / "typeof" __ "(" __ Exp __ ")"
  / TableType
  / FunctionType
  / "(" __ Type __ ")"

SingletonType
  = value:String {
    return {
      type: 'SingletonType',
      value,
      loc: location(),
    };
  }
  / value:("true" / "false") {
    return {
      type: 'SingletonType',
      value: value === 'true',
      loc: location(),
    };
  }

Type
  = SimpleType // TODO: UnionSuffix / IntersectionSuffix

GenericTypePackParameter
  = NAME _ "..."

GenericTypeList
  = NAME (__ "," __ GenericTypeList)?
  / GenericTypePackParameter (__ "," __ GenericTypePackParameter)*

GenericTypePackParameterWithDefault
  = NAME _ "..." __ "=" __ (TypePack / VariadicTypePack / GenericTypePack)

GenericTypeListWithDefaults
  = GenericTypeList ( __ "," __ GenericTypePackParameterWithDefault)*
  / NAME
    ( __ "," __ NAME __ !"=")*
    ( __ "," __ NAME __ "=" __ Type )*
    ( __ "," __ GenericTypePackParameterWithDefault )*
  / NAME __ "=" __ Type 
    ( __ "," __ GenericTypePackParameterWithDefault )*
  / GenericTypePackParameterWithDefault
    ( __ "," __ GenericTypePackParameterWithDefault )*

TypeList
  = head:Type tail:( __ "," __ t:TypeList { return t; })? {
    return tail ? [head, ...tail] : [head];
  }
  / "..." __ type:Type {
    return [{...type, spread: true, loc: location()}];
  }

BoundTypeList
  = (NAME ":")? Type (__ "," __ BoundTypeList)?
  / "..." Type

TypeParams
  = (Type / TypePack / VariadicTypePack / GenericTypePack) (__ "," __ TypeParams)?

TypePack
  = "(" __ t:TypeList? __ ")" {
    return t ?? [];
  }

GenericTypePack
  = NAME "..."

VariadicTypePack
  = "..." Type

ReturnType
  = t:Type { return [t]; }
  / t:TypePack { return t; }

TableIndexer
  = "[" __ keyType:Type __ "]" __ ":" __ valueType:Type {
    return {
      type: 'TableIndexer',
      keyType,
      valueType,
      loc: location(),
    };
  }

TableProp
  = key:NAME __ ":" __ valueType:Type {
    return {
      type: 'TableProp',
      key,
      valueType,
      loc: location(),
    };
  }

TablePropOrIndexer
  = prop:TableProp { return prop; }
  / indexer:TableIndexer { return indexer}

PropList
  = head:TablePropOrIndexer
    tail:(__ FieldSep __ p:TablePropOrIndexer { return p; })* FieldSep? {
      return [head, ...tail];
    }

TableType
  = "{" __ valueType:Type __ "}" {
    return {
      type: 'TableType',
      props: [],
      indexes: [{
        keyType: {
          type: 'TypeRef',
          base: 'number',
          props: []
        },
        valueType,
      }],
      loc: location(),
    };
  }
  / "{" __ propOrIndexes:PropList? __ "}" {
    const props = [];
    const indexes = [];

    for (const propOrIndex of propOrIndexes) {
      if (propOrIndex.type === 'TableIndexer') {
        indexes.push({
          keyType: propOrIndex.keyType,
          valueType: propOrIndex.valueType,
        });
      } else {
        props.push({
          key: propOrIndex.key,
          valueType: propOrIndex.valueType,
        });
      }
    }

    return {
      type: 'TableType',
      props,
      indexes,
      loc: location(),
    };
  }

FunctionType
  = ("<")

// 2.1 Lexical Conventions

NAME = !ReservedWord head:[a-zA-Z_] tail:[a-zA-Z0-9_]* {
    return `${head}${tail.join('')}`;
  }

Number = HexLiteral / DecimalLiteral

HexLiteral
  = "0x" digits:[0-9a-f]+ {
    return parseInt(digits.join(''), 16);
  }

DecimalLiteral
  = n:[0-9]+ {
    return +n.join('');
  }

String = "\"" "\""

ReservedWord
  = rw:("and" /       "break" /     "do"  /       "else" /      "elseif"
  / "end" /       "false" /     "for"  /      "function" /  "if"
  / "in" /        "local" /     "nil"  /      "not" /       "or"
  / "repeat" /    "return" /    "then"  /     "true" /      "until"
  /     "while") ![a-z] { return rw; }

SourceCharacter
  = .

// Separator, Space
Zs = [\u0020\u00A0\u1680\u2000-\u200A\u202F\u205F\u3000]

WhiteSpace "whitespace"
  = "\t"
  / "\v"
  / "\f"
  / " "
  / "\u00A0"
  / "\uFEFF"
  / Zs

LineTerminator
  = [\n\r\u2028\u2029]

LineTerminatorSequence "end of line"
  = "\n"
  / "\r\n"
  / "\r"
  / "\u2028"
  / "\u2029"

Comment "comment"
  = MultiLineComment
  / SingleLineComment

MultiLineComment
  = "/*" (!"*/" SourceCharacter)* "*/"

MultiLineCommentNoLineTerminator
  = "/*" (!("*/" / LineTerminator) SourceCharacter)* "*/"

SingleLineComment
  = "//" (!LineTerminator SourceCharacter)*

__
  = (WhiteSpace / LineTerminatorSequence / Comment)*

_
  = (WhiteSpace / MultiLineCommentNoLineTerminator)*
