open Query

open Js.String2

exception SyntaxError(string)

let isFunction = (str, function) => str->startsWith(`${function}(`) && str->endsWith(")")
let getContent = (str, function) => {
  str
  ->slice(~from=`${function}(`->length, ~to_=-1)
  ->replaceByRe(%re("/^\s+/g"), "")
  ->replaceByRe(%re("/\s+$/g"), "")
}
let isString = str => {
  (str->startsWith(`"`) && str->endsWith(`"`)) ||
  str->startsWith(`'`) && str->endsWith(`'`) ||
  (str->startsWith("`") && str->endsWith("`"))
}

/**
function parse(str) {
  let result = [], item = '', depth = 0;

  function push() { if (item) result.push(item); item = ''; }

  for (let i = 0, c; c = str[i], i < str.length; i++) {
    if (!depth && c === ',') push();
    else {
      item += c;
      if (c === '[') depth++;
      if (c === ']') depth--;
    }
  }
  
  push();
  return result;
}
        
console.log(parse("[1, '15', [false]], [[], 'sup']"));
*/
let matchSplitByComma = str => {
  let result = []
  let item = ref("")
  let depth = ref(0)
  let stringQuote = ref("")

  let push = () => {
    if item.contents !== "" {
      Js.Array2.push(result, item.contents)->ignore
      item := ""
    }
  }

  for i in 0 to str->Js.String2.length {
    let c = str->Js.String2.charAt(i)
    if depth.contents === 0 && c === "," && stringQuote.contents === "" {
      push()
    } else {
      item := item.contents ++ c
      switch c {
      | "("
      | "["
      | "{" =>
        depth := depth.contents + 1
      | ")"
      | "]"
      | "}" =>
        depth := depth.contents - 1
      | str =>
        if stringQuote.contents === "" {
          switch c {
          | `"`
          | `'`
          | "`" =>
            stringQuote := c
          | _ => ()
          }
        } else {
          let cBefore = {
            if i !== 0 {
              str->Js.String2.charAt(i - 1)
            } else {
              ""
            }
          }
          if cBefore !== "\"" {
            switch (stringQuote.contents, c) {
            | ("\"", "\"")
            | ("'", "'")
            | ("`", "`") =>
              stringQuote := ""
            | _ => ()
            }
          }
        }
      }
    }
  }
  push()

  Some(result)
}

// FQL make
let rec make = str => {
  let trimmed = str->replaceByRe(%re("/^\s+/g"), "")->replaceByRe(%re("/\s+$/g"), "")
  let whitespaceRemoved = {
    // if "string", not remove whitespace
    switch trimmed {
    | str if str->isString => str
    | str if str->startsWith(`[`) && str->endsWith(`]`) => str
    | str if str->startsWith(`{`) && str->endsWith(`}`) => str
    | str if str |> Js.Re.test_(%re("/^[A-Z][a-zA-Z]+[\s]{0,}\(.*\)$/gm")) =>
      // functions
      trimmed->replaceByRe(%re("/[\s](?=[^\)]*?(?:\(|$))/g"), "")
    | str => str
    }
  }

  switch whitespaceRemoved {
  // primitives
  | "false" => False
  | "true" => True
  | "null" => Null
  | str if str->isString => {
      // remove quotes start and end
      let content = str->slice(~from=1, ~to_=-1)
      String(content)
    }
  | str if str |> Js.Re.test_(%re("/^[-]?\d+$/")) => Int(str->int_of_string)
  | str if str |> Js.Re.test_(%re("/^[-]?\d+(\.\d+)?$/")) => Float(str->Js.Float.fromString)
  | str if str->startsWith(`[`) && str->endsWith(`]`) => {
      let content = str->slice(~from=1, ~to_=-1)
      let result = content->matchSplitByComma

      // Append()

      switch result {
      | Some(result) =>
        result
        ->Js.Array2.map(item => {
          make(item)
        })
        ->Array
      | None => Array([content->make])
      }
    }
  | str if str->startsWith(`{`) && str->endsWith(`}`) => {
      let content = str->slice(~from=1, ~to_=-1)
      let result = content->matchSplitByComma

      // Append()
      switch result {
      | Some(result) => {
          let newResult = result->Js.Array2.map(item => {
            // make(item)
            let trimmed = item->replaceByRe(%re("/^\s+/g"), "")->replaceByRe(%re("/\s+$/g"), "")
            trimmed
          })
          Js.log(newResult)
          let entries = Belt.Array.make(newResult->Js.Array2.length / 2, "")->Js.Array2.mapi((
            _,
            i,
          ) => {
            let key = newResult[i * 2]
            let val = newResult[i * 2 + 1]->replaceByRe(%re("/^:\s+/g"), "")
            Js.log2(key, val)
            (key, val->make)
          })
          entries->Js.Dict.fromArray->Dict
        }
      | None => Null
      }
    }

  | str if str->isFunction("Abort") => {
      let content = str->getContent("Abort")
      Abort(content->make)
    }
  | str if str->isFunction("Abs") => {
      let content = str->getContent("Abs")
      Abs(content->make)
    }
  | str if str->isFunction("AccessProvider") => {
      let content = str->getContent("AccessProvider")
      AccessProvider(content->make)
    }
  | str if str->isFunction("AccessProviders") => {
      let content = str->getContent("AccessProviders")
      switch content {
      | "" => AccessProviders()
      | _ => raise(SyntaxError("cannot have args:" ++ str))
      }
    }

  | str if str->isFunction("Acos") => {
      let content = str->getContent("Acos")
      Acos(content->make)
    }
  | str if str->isFunction("Add") => {
      let content = str->getContent("Add")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ item: " ++ str))
      | Some(arr) => Add(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("All") => {
      let content = str->getContent("All")
      All(content->make)
    }
  | str if str->isFunction("And") => {
      let content = str->getContent("And")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ item: " ++ str))
      | Some(arr) => And(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("Any") => {
      let content = str->getContent("Any")
      Any(content->make)
    }
  | str if str->isFunction("Append") => {
      let content = str->getContent("Append")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Append(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args:" ++ str))
      }
    }
  | str if str->isFunction("Asin") => {
      let content = str->getContent("Asin")
      Asin(content->make)
    }
  | str if str->isFunction("At") => {
      let content = str->getContent("At")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => At(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args:" ++ str))
      }
    }
  | str if str->isFunction("Atan") => {
      let content = str->getContent("Atan")
      Atan(content->make)
    }
  | str if str->isFunction("BitAnd") => {
      let content = str->getContent("BitAnd")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+: " ++ str))
      | Some(arr) => BitAnd(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("BitNot") => {
      let content = str->getContent("BitNot")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+: " ++ str))
      | Some(arr) => BitNot(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("BitOr") => {
      let content = str->getContent("BitOr")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+: " ++ str))
      | Some(arr) => BitOr(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("BitXor") => {
      let content = str->getContent("BitXor")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+: " ++ str))
      | Some(arr) => BitXor(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("Call") => {
      let content = str->getContent("Call")
      switch content->matchSplitByComma {
      | Some([])
      | None =>
        raise(SyntaxError("must 1+ args:" ++ str))
      | Some([q1, q2]) => Call(q1->make, q2->make)
      | Some(qArr) => {
          let qRest = qArr->Js.Array2.sliceFrom(1)
          Call(qArr[0]->make, Array(qRest->Js.Array2.map(item => item->make)))
        }
      }
    }
  | str if str->isFunction("CaseFold") => {
      let content = str->getContent("CaseFold")
      switch content->matchSplitByComma {
      | Some([q1]) => CaseFold(q1->make, None)
      | Some([q1, q2]) => CaseFold(q1->make, Some(q2->make))
      | _ => raise(SyntaxError("must 1/2 args:" ++ str))
      }
    }
  | str if str->isFunction("Ceil") => {
      let content = str->getContent("Ceil")
      Ceil(content->make)
    }
  | str if str->isFunction("Collection") => {
      let content = str->getContent("Collection")
      switch content->matchSplitByComma {
      | Some([q1]) => Collection(q1->make, None)
      | Some([q1, q2]) => Collection(q1->make, Some(q2->make))
      | _ => raise(SyntaxError("must 1/2 args:" ++ str))
      }
    }
  | str if str->isFunction("Collections") => {
      let content = str->getContent("Collections")
      switch content {
      | "" => Collections(None)
      | content => Collections(Some(content->make))
      }
    }
  | str if str->isFunction("Concat") => {
      let content = str->getContent("Concat")
      switch content->matchSplitByComma {
      | Some([q1]) => Concat(q1->make, None)
      | Some([q1, q2]) => Concat(q1->make, Some(q2->make))
      | _ => raise(SyntaxError("must 1/2 args:" ++ str))
      }
    }
  | str if str->isFunction("ContainsField") => {
      let content = str->getContent("ContainsField")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => ContainsField(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args:" ++ str))
      }
    }
  | str if str->isFunction("ContainsPath") => {
      let content = str->getContent("ContainsPath")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => ContainsPath(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args:" ++ str))
      }
    }
  | str if str->isFunction("ContainsStr") => {
      let content = str->getContent("ContainsStr")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => ContainsStr(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args:" ++ str))
      }
    }
  | str if str->isFunction("ContainsStrRegex") => {
      let content = str->getContent("ContainsStrRegex")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => ContainsStrRegex(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args:" ++ str))
      }
    }
  | str if str->isFunction("ContainsValue") => {
      let content = str->getContent("ContainsValue")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => ContainsValue(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args:" ++ str))
      }
    }
  | str if str->isFunction("Cos") => {
      let content = str->getContent("Cos")
      Cos(content->make)
    }
  | str if str->isFunction("Cosh") => {
      let content = str->getContent("Cosh")
      Cosh(content->make)
    }
  | str if str->isFunction("Count") => {
      let content = str->getContent("Count")
      Count(content->make)
    }
  | str if str->isFunction("Create") => {
      let content = str->getContent("Create")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Create(q1->make, Object(q2->make))
      | _ => raise(SyntaxError("must 2 args:" ++ str))
      }
    }
  | str if str->isFunction("CreateAccessProvider") => {
      let content = str->getContent("CreateAccessProvider")
      CreateAccessProvider(Object(content->make))
    }
  | str if str->isFunction("CreateCollection") => {
      let content = str->getContent("CreateCollection")
      CreateCollection(Object(content->make))
    }
  | str if str->isFunction("CreateDatabase") => {
      let content = str->getContent("CreateDatabase")
      CreateDatabase(Object(content->make))
    }
  | str if str->isFunction("CreateFunction") => {
      let content = str->getContent("CreateFunction")
      CreateFunction(Object(content->make))
    }
  | str if str->isFunction("CreateIndex") => {
      let content = str->getContent("CreateIndex")
      CreateIndex(Object(content->make))
    }
  | str if str->isFunction("CreateKey") => {
      let content = str->getContent("CreateKey")
      CreateKey(Object(content->make))
    }
  | str if str->isFunction("CreateRole") => {
      let content = str->getContent("CreateRole")
      CreateRole(Object(content->make))
    }
  | str if str->isFunction("Credentials") => {
      let content = str->getContent("Credentials")
      switch content {
      | "" => Credentials(None)
      | content => Credentials(Some(content->make))
      }
    }
  | str if str->isFunction("CurrentIdentity") => {
      let content = str->getContent("CurrentIdentity")
      switch content {
      | "" => CurrentIdentity()
      | _ => raise(SyntaxError("cannot have args:" ++ str))
      }
    }
  | str if str->isFunction("CurrentToken") => {
      let content = str->getContent("CurrentToken")
      switch content {
      | "" => CurrentToken()
      | _ => raise(SyntaxError("cannot have args:" ++ str))
      }
    }
  | str if str->isFunction("CurrentToken") => {
      let content = str->getContent("CurrentToken")
      switch content {
      | "" => CurrentToken()
      | _ => raise(SyntaxError("cannot have args:" ++ str))
      }
    }

  | str if str->isFunction("Database") => {
      let content = str->getContent("Database")
      switch content->matchSplitByComma {
      | Some([q1]) => Database(q1->make, None)
      | Some([q1, q2]) => Database(q1->make, Some(q2->make))
      | _ => raise(SyntaxError("must 1/2 args:" ++ str))
      }
    }
  | str if str->isFunction("Databases") => {
      let content = str->getContent("Databases")
      switch content {
      | "" => Databases(None)
      | content => Databases(Some(content->make))
      }
    }
  | str if str->isFunction("Date") => {
      let content = str->getContent("Date")
      Date(content->make)
    }
  | str if str->isFunction("DayOfMonth") => {
      let content = str->getContent("DayOfMonth")
      DayOfMonth(content->make)
    }
  | str if str->isFunction("DayOfWeek") => {
      let content = str->getContent("DayOfWeek")
      DayOfWeek(content->make)
    }
  | str if str->isFunction("DayOfYear") => {
      let content = str->getContent("DayOfYear")
      DayOfYear(content->make)
    }
  | str if str->isFunction("Degrees") => {
      let content = str->getContent("Degrees")
      Degrees(content->make)
    }
  | str if str->isFunction("Delete") => {
      let content = str->getContent("Delete")
      Delete(content->make)
    }
  | str if str->isFunction("Difference") => {
      let content = str->getContent("Difference")
      switch content->matchSplitByComma {
      | Some([])
      | Some([_])
      | None =>
        raise(SyntaxError("must 2+ item: " ++ str))
      | Some(arr) => Difference(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("Distinct") => {
      let content = str->getContent("Distinct")
      Distinct(content->make)
    }
  | str if str->isFunction("Divide") => {
      let content = str->getContent("Divide")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ item: " ++ str))
      | Some(arr) => Divide(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("Do") => {
      let content = str->getContent("Do")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ item: " ++ str))
      | Some(arr) => Do(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("Documents") => {
      let content = str->getContent("Documents")
      Documents(content->make)
    }
  | str if str->isFunction("Drop") => {
      let content = str->getContent("Drop")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Drop(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args:" ++ str))
      }
    }
  | str if str->isFunction("EndsWith") => {
      let content = str->getContent("EndsWith")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => EndsWith(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args:" ++ str))
      }
    }
  | str if str->isFunction("Epoch") => {
      let content = str->getContent("Epoch")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Epoch(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args:" ++ str))
      }
    }
  | str if str->isFunction("Epoch") => {
      let content = str->getContent("Epoch")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Epoch(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args:" ++ str))
      }
    }
  | str if str->isFunction("Equals") => {
      let content = str->getContent("Equals")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ args: " ++ str))
      | Some(arr) => Equals(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("Events") => {
      let content = str->getContent("Events")
      Events(content->make)
    }
  | str if str->isFunction("Exists") => {
      let content = str->getContent("Exists")
      switch content->matchSplitByComma {
      | Some([q1]) => Exists(q1->make, None)
      | Some([q1, q2]) => Exists(q1->make, Some(q2->make))
      | _ => raise(SyntaxError("must 1/2 args:" ++ str))
      }
    }
  | str if str->isFunction("Exp") => {
      let content = str->getContent("Exp")
      Exp(content->make)
    }
  | str if str->isFunction("Filter") => {
      let content = str->getContent("Filter")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Filter(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args:" ++ str))
      }
    }
  | str if str->isFunction("FindStr") => {
      let content = str->getContent("FindStr")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => FindStr(q1->make, q2->make, None)
      | Some([q1, q2, q3]) => FindStr(q1->make, q2->make, Some(q3->make))
      | _ => raise(SyntaxError("must 2+ args:" ++ str))
      }
    }
  | str if str->isFunction("FindStrRegex") => {
      let content = str->getContent("FindStrRegex")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => FindStrRegex(q1->make, q2->make, None, None)
      | Some([q1, q2, q3]) => FindStrRegex(q1->make, q2->make, Some(q3->make), None)
      | Some([q1, q2, q3, q4]) => FindStrRegex(q1->make, q2->make, Some(q3->make), Some(q4->make))
      | _ => raise(SyntaxError("must 2-4 args:" ++ str))
      }
    }
  | str if str->isFunction("Floor") => {
      let content = str->getContent("Floor")
      Floor(content->make)
    }
  | str if str->isFunction("Foreach") => {
      let content = str->getContent("Foreach")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Foreach(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args:" ++ str))
      }
    }

  | str if str->isFunction("Format") => {
      let content = str->getContent("Format")
      switch content->matchSplitByComma {
      | Some([])
      | None =>
        raise(SyntaxError("must 1+ args:" ++ str))
      | Some([q1]) => Format(q1->make, None)
      | Some([q1, q2]) => Format(q1->make, Some(q2->make))
      | Some(qArr) => {
          let qRest = qArr->Js.Array2.sliceFrom(1)
          Format(qArr[0]->make, Some(Array(qRest->Js.Array2.map(item => item->make))))
        }
      }
    }
  | str if str->isFunction("Function") => {
      let content = str->getContent("Function")
      switch content->matchSplitByComma {
      | Some([q1]) => Function(q1->make, None)
      | Some([q1, q2]) => Function(q1->make, Some(q2->make))
      | _ => raise(SyntaxError("must 1+ args: " ++ str))
      }
    }
  | str if str->isFunction("Functions") => {
      let content = str->getContent("Functions")
      switch content {
      | "" => Functions(None)
      | str => Functions(Some(str->make))
      }
    }
  | str if str->isFunction("GT") => {
      let content = str->getContent("GT")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ args:" ++ str))
      | Some(arr) => GT(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("GTE") => {
      let content = str->getContent("GTE")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ args:" ++ str))
      | Some(arr) => GTE(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("Get") => {
      let content = str->getContent("Get")
      switch content->matchSplitByComma {
      | Some([q1]) => Get(q1->make, None)
      | Some([q1, q2]) => Get(q1->make, Some(q2->make))
      | _ => raise(SyntaxError("must 1+ args: " ++ str))
      }
    }
  | str if str->isFunction("HasCurrentIdentity") => {
      let content = str->getContent("HasCurrentIdentity")
      switch content {
      | "" => HasCurrentIdentity()
      | _ => raise(SyntaxError("must 0 args: " ++ str))
      }
    }
  | str if str->isFunction("HasCurrentToken") => {
      let content = str->getContent("HasCurrentToken")
      switch content {
      | "" => HasCurrentToken()
      | _ => raise(SyntaxError("must 0 args: " ++ str))
      }
    }
  | str if str->isFunction("Hour") => {
      let content = str->getContent("Hour")
      Hour(content->make)
    }
  | str if str->isFunction("Hypot") => {
      let content = str->getContent("Hypot")
      switch content->matchSplitByComma {
      | Some([q1]) => Hypot(q1->make, None)
      | Some([q1, q2]) => Hypot(q1->make, Some(q2->make))
      | _ => raise(SyntaxError("must 1-2 args:" ++ str))
      }
    }
  | str if str->isFunction("Identify") => {
      let content = str->getContent("Identify")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Identify(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args:" ++ str))
      }
    }
  | str if str->isFunction("If") => {
      let content = str->getContent("If")
      switch content->matchSplitByComma {
      | Some([q1, q2, q3]) => If(q1->make, q2->make, q3->make)
      | _ => raise(SyntaxError("must 3 args:" ++ str))
      }
    }
  | str if str->isFunction("Index") => {
      let content = str->getContent("Index")
      switch content->matchSplitByComma {
      | Some([q1]) => Index(q1->make, None)
      | Some([q1, q2]) => Index(q1->make, Some(q2->make))
      | _ => raise(SyntaxError("must 1-2 args:" ++ str))
      }
    }
  | str if str->isFunction("Indexes") => {
      let content = str->getContent("Indexes")
      switch content {
      | "" => Indexes(None)
      | q1 => Indexes(Some(q1->make))
      }
    }
  | str if str->isFunction("Insert") => {
      let content = str->getContent("Insert")
      switch content->matchSplitByComma {
      | Some([q1, q2, q3, q4]) => Insert(q1->make, q2->make, q3->make, q4->make)
      | _ => raise(SyntaxError("must 4 args:" ++ str))
      }
    }
  | str if str->isFunction("Intersection") => {
      let content = str->getContent("Intersection")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ args:" ++ str))
      | Some(arr) => Intersection(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("IsArray") => {
      let content = str->getContent("IsArray")
      IsArray(content->make)
    }
  | str if str->isFunction("IsBoolean") => {
      let content = str->getContent("IsBoolean")
      IsBoolean(content->make)
    }
  | str if str->isFunction("IsBytes") => {
      let content = str->getContent("IsBytes")
      IsBytes(content->make)
    }
  | str if str->isFunction("IsCollection") => {
      let content = str->getContent("IsCollection")
      IsCollection(content->make)
    }
  | str if str->isFunction("IsCredentials") => {
      let content = str->getContent("IsCredentials")
      IsCredentials(content->make)
    }
  | str if str->isFunction("IsDatabase") => {
      let content = str->getContent("IsDatabase")
      IsDatabase(content->make)
    }
  | str if str->isFunction("IsDate") => {
      let content = str->getContent("IsDate")
      IsDate(content->make)
    }
  | str if str->isFunction("IsDoc") => {
      let content = str->getContent("IsDoc")
      IsDoc(content->make)
    }
  | str if str->isFunction("IsDouble") => {
      let content = str->getContent("IsDouble")
      IsDouble(content->make)
    }
  | str if str->isFunction("IsEmpty") => {
      let content = str->getContent("IsEmpty")
      IsEmpty(content->make)
    }
  | str if str->isFunction("IsFunction") => {
      let content = str->getContent("IsFunction")
      IsFunction(content->make)
    }
  | str if str->isFunction("IsIndex") => {
      let content = str->getContent("IsIndex")
      IsIndex(content->make)
    }
  | str if str->isFunction("IsInteger") => {
      let content = str->getContent("IsInteger")
      IsInteger(content->make)
    }
  | str if str->isFunction("IsKey") => {
      let content = str->getContent("IsKey")
      IsKey(content->make)
    }
  | str if str->isFunction("IsLambda") => {
      let content = str->getContent("IsLambda")
      IsLambda(content->make)
    }
  | str if str->isFunction("IsNonEmpty") => {
      let content = str->getContent("IsNonEmpty")
      IsNonEmpty(content->make)
    }
  | str if str->isFunction("IsNull") => {
      let content = str->getContent("IsNull")
      IsNull(content->make)
    }
  | str if str->isFunction("IsNumber") => {
      let content = str->getContent("IsNumber")
      IsNumber(content->make)
    }
  | str if str->isFunction("IsObject") => {
      let content = str->getContent("IsObject")
      IsObject(content->make)
    }
  | str if str->isFunction("IsRef") => {
      let content = str->getContent("IsRef")
      IsRef(content->make)
    }
  | str if str->isFunction("IsRole") => {
      let content = str->getContent("IsRole")
      IsRole(content->make)
    }
  | str if str->isFunction("IsSet") => {
      let content = str->getContent("IsSet")
      IsSet(content->make)
    }
  | str if str->isFunction("IsString") => {
      let content = str->getContent("IsString")
      IsString(content->make)
    }
  | str if str->isFunction("IsTimestamp") => {
      let content = str->getContent("IsTimestamp")
      IsTimestamp(content->make)
    }
  | str if str->isFunction("IsToken") => {
      let content = str->getContent("IsToken")
      IsToken(content->make)
    }
  | str if str->isFunction("Join") => {
      let content = str->getContent("Join")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Join(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args: " ++ str))
      }
    }
  | str if str->isFunction("KeyFromSecret") => {
      let content = str->getContent("KeyFromSecret")
      KeyFromSecret(content->make)
    }
  | str if str->isFunction("Keys") => {
      let content = str->getContent("Keys")
      switch content {
      | "" => Keys(None)
      | str => Keys(Some(str->make))
      }
    }
  | str if str->isFunction("LT") => {
      let content = str->getContent("LT")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ args:" ++ str))
      | Some(arr) => LT(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("LTE") => {
      let content = str->getContent("LTE")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ args:" ++ str))
      | Some(arr) => LTE(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("LTrim") => {
      let content = str->getContent("LTrim")
      LTrim(content->make)
    }
  | str if str->isFunction("Lambda") => {
      let content = str->getContent("Lambda")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Lambda(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args: " ++ str))
      }
    }
  | str if str->isFunction("Length") => {
      let content = str->getContent("Length")
      Length(content->make)
    }
  | str if str->isFunction("Let") => {
      let content = str->getContent("Let")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Let(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args: " ++ str))
      }
    }
  | str if str->isFunction("Ln") => {
      let content = str->getContent("Ln")
      Ln(content->make)
    }
  | str if str->isFunction("Log") => {
      let content = str->getContent("Log")
      Log(content->make)
    }
  | str if str->isFunction("Login") => {
      let content = str->getContent("Login")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Login(q1->make, Object(q2->make))
      | _ => raise(SyntaxError("must 2 args: " ++ str))
      }
    }
  | str if str->isFunction("Logout") => {
      let content = str->getContent("Logout")
      Logout(content->make)
    }
  | str if str->isFunction("LowerCase") => {
      let content = str->getContent("LowerCase")
      LowerCase(content->make)
    }
  | str if str->isFunction("Map") => {
      let content = str->getContent("Map")
      Js.log(content->matchSplitByComma)
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Map(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args: " ++ str))
      }
    }
  | str if str->isFunction("Match") => {
      let content = str->getContent("Match")
      switch content->matchSplitByComma {
      | Some([q1]) => Match(q1->make, None)
      | Some([q1, q2]) => Match(q1->make, Some(q2->make))
      | _ => raise(SyntaxError("must 1-2 args: " ++ str))
      }
    }
  | str if str->isFunction("Max") => {
      let content = str->getContent("Max")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ args:" ++ str))
      | Some(arr) => Max(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("Mean") => {
      let content = str->getContent("Mean")
      Mean(content->make)
    }
  | str if str->isFunction("Merge") => {
      let content = str->getContent("Merge")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Merge(q1->make, q2->make, None)
      | Some([q1, q2, q3]) => Merge(q1->make, q2->make, Some(q3->make))
      | _ => raise(SyntaxError("must 2-3 args: " ++ str))
      }
    }
  | str if str->isFunction("Min") => {
      let content = str->getContent("Min")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ args:" ++ str))
      | Some(arr) => Min(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("Minute") => {
      let content = str->getContent("Minute")
      Minute(content->make)
    }
  | str if str->isFunction("Modulo") => {
      let content = str->getContent("Modulo")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ args:" ++ str))
      | Some(arr) => Modulo(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("Month") => {
      let content = str->getContent("Month")
      Month(content->make)
    }
  | str if str->isFunction("MoveDatabase") => {
      let content = str->getContent("MoveDatabase")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => MoveDatabase(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args: " ++ str))
      }
    }
  | str if str->isFunction("Multiply") => {
      let content = str->getContent("Multiply")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ args:" ++ str))
      | Some(arr) => Multiply(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("NewId") => {
      let content = str->getContent("NewId")
      switch content {
      | "" => NewId()
      | str => raise(SyntaxError("cannot have args: " ++ str))
      }
    }
  | str if str->isFunction("Not") => {
      let content = str->getContent("Not")
      Not(content->make)
    }
  | str if str->isFunction("Now") => {
      let content = str->getContent("Now")
      switch content {
      | "" => Now()
      | str => raise(SyntaxError("cannot have args: " ++ str))
      }
    }
  | str if str->isFunction("Or") => {
      let content = str->getContent("Or")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ args:" ++ str))
      | Some(arr) => Or(Array(arr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("Paginate") => {
      let content = str->getContent("Paginate")
      switch content->matchSplitByComma {
      | Some([q1]) => Paginate(q1->make, None)
      | Some([q1, q2]) => Paginate(q1->make, Some(q2->make))
      | _ => raise(SyntaxError("must 1-2 args: " ++ str))
      }
    }
  | str if str->isFunction("Pow") => {
      let content = str->getContent("Pow")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Pow(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args: " ++ str))
      }
    }
  | str if str->isFunction("Prepend") => {
      let content = str->getContent("Prepend")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Prepend(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args: " ++ str))
      }
    }
  | str if str->isFunction("Query") => {
      let content = str->getContent("Query")
      Query(content->make)
    }
  | str if str->isFunction("RTrim") => {
      let content = str->getContent("RTrim")
      RTrim(content->make)
    }
  | str if str->isFunction("Radians") => {
      let content = str->getContent("Radians")
      Radians(content->make)
    }
  | str if str->isFunction("Range") => {
      let content = str->getContent("Range")
      switch content->matchSplitByComma {
      | Some([q1, q2, q3]) => Range(q1->make, q2->make, q3->make)
      | _ => raise(SyntaxError("must 3 args: " ++ str))
      }
    }
  | str if str->isFunction("Reduce") => {
      let content = str->getContent("Reduce")
      switch content->matchSplitByComma {
      | Some([q1, q2, q3]) => Reduce(q1->make, q2->make, q3->make)
      | _ => raise(SyntaxError("must 3 args: " ++ str))
      }
    }
  | str if str->isFunction("Ref") => {
      let content = str->getContent("Ref")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Ref(q1->make, q2->make)
      | _ => raise(SyntaxError("must 3 args: " ++ str))
      }
    }
  | str if str->isFunction("RegexEscape") => {
      let content = str->getContent("RegexEscape")
      RegexEscape(content->make)
    }
  | str if str->isFunction("Remove") => {
      let content = str->getContent("Remove")
      switch content->matchSplitByComma {
      | Some([q1, q2, q3]) => Remove(q1->make, q2->make, q3->make)
      | _ => raise(SyntaxError("must 3 args: " ++ str))
      }
    }
  | str if str->isFunction("Repeat") => {
      let content = str->getContent("Repeat")
      switch content->matchSplitByComma {
      | Some([q1]) => Repeat(q1->make, None)
      | Some([q1, q2]) => Repeat(q1->make, Some(q2->make))
      | _ => raise(SyntaxError("must 3 args: " ++ str))
      }
    }
  | str if str->isFunction("Replace") => {
      let content = str->getContent("Replace")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Replace(q1->make, Object(q2->make))
      | _ => raise(SyntaxError("must 3 args: " ++ str))
      }
    }
  | str if str->isFunction("ReplaceStr") => {
      let content = str->getContent("ReplaceStr")
      switch content->matchSplitByComma {
      | Some([q1, q2, q3]) => ReplaceStr(q1->make, q2->make, q3->make)
      | _ => raise(SyntaxError("must 3 args: " ++ str))
      }
    }
  | str if str->isFunction("ReplaceStrRegex") => {
      let content = str->getContent("ReplaceStrRegex")
      switch content->matchSplitByComma {
      | Some([q1, q2, q3]) => ReplaceStrRegex(q1->make, q2->make, q3->make, None)
      | Some([q1, q2, q3, q4]) => ReplaceStrRegex(q1->make, q2->make, q3->make, Some(q4->make))
      | _ => raise(SyntaxError("must 3-4 args: " ++ str))
      }
    }
  | str if str->isFunction("ReplaceStrRegex") => {
      let content = str->getContent("ReplaceStrRegex")
      switch content->matchSplitByComma {
      | Some([q1, q2, q3]) => ReplaceStrRegex(q1->make, q2->make, q3->make, None)
      | Some([q1, q2, q3, q4]) => ReplaceStrRegex(q1->make, q2->make, q3->make, Some(q4->make))
      | _ => raise(SyntaxError("must 3-4 args: " ++ str))
      }
    }
  | str if str->isFunction("Reverse") => {
      let content = str->getContent("Reverse")
      RegexEscape(content->make)
    }
  | str if str->isFunction("Role") => {
      let content = str->getContent("Role")
      switch content->matchSplitByComma {
      | Some([q1]) => Role(q1->make, None)
      | Some([q1, q2]) => Role(q1->make, Some(q2->make))
      | _ => raise(SyntaxError("must 1-2 args: " ++ str))
      }
    }
  | str if str->isFunction("Roles") => {
      let content = str->getContent("Roles")
      switch content {
      | "" => Roles(None)
      | q1 => Roles(Some(q1->make))
      }
    }
  | str if str->isFunction("Round") => {
      let content = str->getContent("Round")
      switch content->matchSplitByComma {
      | Some([q1]) => Round(q1->make, None)
      | Some([q1, q2]) => Round(q1->make, Some(q2->make))
      | _ => raise(SyntaxError("must 1-2 args: " ++ str))
      }
    }
  | str if str->isFunction("Second") => {
      let content = str->getContent("Second")
      Second(content->make)
    }
  | str if str->isFunction("Select") => {
      let content = str->getContent("Select")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Select(q1->make, q2->make, None)
      | Some([q1, q2, q3]) => Select(q1->make, q2->make, Some(q3->make))
      | _ => raise(SyntaxError("must 3 args: " ++ str))
      }
    }
  | str if str->isFunction("Sign") => {
      let content = str->getContent("Sign")
      Sign(content->make)
    }
  | str if str->isFunction("Sin") => {
      let content = str->getContent("Sin")
      Sin(content->make)
    }
  | str if str->isFunction("Singleton") => {
      let content = str->getContent("Singleton")
      Singleton(content->make)
    }
  | str if str->isFunction("Sinh") => {
      let content = str->getContent("Sinh")
      Sinh(content->make)
    }
  | str if str->isFunction("Space") => {
      let content = str->getContent("Space")
      Space(content->make)
    }
  | str if str->isFunction("Sqrt") => {
      let content = str->getContent("Sqrt")
      Sqrt(content->make)
    }
  | str if str->isFunction("StartsWith") => {
      let content = str->getContent("StartsWith")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => StartsWith(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args: " ++ str))
      }
    }
  | str if str->isFunction("SubString") => {
      let content = str->getContent("SubString")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => SubString(q1->make, q2->make, None)
      | Some([q1, q2, q3]) => SubString(q1->make, q2->make, Some(q3->make))
      | _ => raise(SyntaxError("must 2 args: " ++ str))
      }
    }
  | str if str->isFunction("Subtract") => {
      let content = str->getContent("Subtract")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ args: " ++ str))
      | Some(qArr) => Subtract(Array(qArr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("Sum") => {
      let content = str->getContent("Sum")
      Sum(content->make)
    }
  | str if str->isFunction("Take") => {
      let content = str->getContent("Take")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Take(q1->make, q2->make)
      | _ => raise(SyntaxError("must 2 args: " ++ str))
      }
    }
  | str if str->isFunction("Tan") => {
      let content = str->getContent("Tan")
      Tan(content->make)
    }
  | str if str->isFunction("Tanh") => {
      let content = str->getContent("Tanh")
      Tanh(content->make)
    }
  | str if str->isFunction("Time") => {
      let content = str->getContent("Time")
      Time(content->make)
    }
  | str if str->isFunction("TimeAdd") => {
      let content = str->getContent("TimeAdd")
      switch content->matchSplitByComma {
      | Some([q1, q2, q3]) => TimeAdd(q1->make, q2->make, q3->make)
      | _ => raise(SyntaxError("must 3 args: " ++ str))
      }
    }
  | str if str->isFunction("TimeDiff") => {
      let content = str->getContent("TimeDiff")
      switch content->matchSplitByComma {
      | Some([q1, q2, q3]) => TimeDiff(q1->make, q2->make, q3->make)
      | _ => raise(SyntaxError("must 3 args: " ++ str))
      }
    }
  | str if str->isFunction("TimeSubtract") => {
      let content = str->getContent("TimeSubtract")
      switch content->matchSplitByComma {
      | Some([q1, q2, q3]) => TimeSubtract(q1->make, q2->make, q3->make)
      | _ => raise(SyntaxError("must 3 args: " ++ str))
      }
    }
  | str if str->isFunction("TitleCase") => {
      let content = str->getContent("TitleCase")
      TitleCase(content->make)
    }
  | str if str->isFunction("ToArray") => {
      let content = str->getContent("ToArray")
      ToArray(content->make)
    }
  | str if str->isFunction("ToDate") => {
      let content = str->getContent("ToDate")
      ToDate(content->make)
    }
  | str if str->isFunction("ToDouble") => {
      let content = str->getContent("ToDouble")
      ToDouble(content->make)
    }
  | str if str->isFunction("ToInteger") => {
      let content = str->getContent("ToInteger")
      ToInteger(content->make)
    }
  | str if str->isFunction("ToMicros") => {
      let content = str->getContent("ToMicros")
      ToMicros(content->make)
    }
  | str if str->isFunction("ToMillis") => {
      let content = str->getContent("ToMillis")
      ToMillis(content->make)
    }
  | str if str->isFunction("ToNumber") => {
      let content = str->getContent("ToNumber")
      ToNumber(content->make)
    }
  | str if str->isFunction("ToObject") => {
      let content = str->getContent("ToObject")
      ToObject(content->make)
    }
  | str if str->isFunction("ToSeconds") => {
      let content = str->getContent("ToSeconds")
      ToSeconds(content->make)
    }
  | str if str->isFunction("ToString") => {
      let content = str->getContent("ToString")
      ToString(content->make)
    }
  | str if str->isFunction("ToTime") => {
      let content = str->getContent("ToTime")
      ToTime(content->make)
    }
  | str if str->isFunction("Tokens") => {
      let content = str->getContent("Tokens")
      switch content {
      | "" => Tokens(None)
      | q1 => Tokens(Some(q1->make))
      }
    }
  | str if str->isFunction("Trim") => {
      let content = str->getContent("Trim")
      Trim(content->make)
    }
  | str if str->isFunction("Trunc") => {
      let content = str->getContent("Trunc")
      switch content->matchSplitByComma {
      | Some([q1]) => Trunc(q1->make, None)
      | Some([q1, q2]) => Trunc(q1->make, Some(q2->make))
      | _ => raise(SyntaxError("must 1-2 args: " ++ str))
      }
    }
  | str if str->isFunction("Union") => {
      let content = str->getContent("Union")
      switch content->matchSplitByComma {
      | None => raise(SyntaxError("must 1+ args: " ++ str))
      | Some(qArr) => Union(Array(qArr->Js.Array2.map(item => item->make)))
      }
    }
  | str if str->isFunction("Update") => {
      let content = str->getContent("Update")
      switch content->matchSplitByComma {
      | Some([q1, q2]) => Update(q1->make, Object(q2->make))
      | _ => raise(SyntaxError("must 2 args: " ++ str))
      }
    }
  | str if str->isFunction("UpperCase") => {
      let content = str->getContent("UpperCase")
      UpperCase(content->make)
    }
  | str if str->isFunction("Var") => {
      let content = str->getContent("Var")
      Var(content->make)
    }
  | str if str->isFunction("Year") => {
      let content = str->getContent("Year")
      Year(content->make)
    }

  | str => {
      Js.Console.error("Syntax not found")
      raise(SyntaxError("Syntax not found: " ++ str))
    }
  }
}
