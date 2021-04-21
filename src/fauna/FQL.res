open Query

open Js.String2

exception SyntaxError(string)

let isFunction = (str, function) => str->startsWith(`${function}(`) && str->endsWith(")")
let getContent = (str, function) => str->slice(~from=`${function}(`->length, ~to_=-1)
let isString = str => {
  (str->startsWith(`"`) && str->endsWith(`"`)) ||
  str->startsWith(`'`) && str->endsWith(`'`) ||
  (str->startsWith("`") && str->endsWith("`"))
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
      let result =
        content->Js.String2.match_(
          %re("/\s*(\"[^\"]*\"|\`[^\`]*\`|\'[^\']*\'|\([^)]*\)|\[[^\]]*\]|\{[^\}]*\}|[^,]+)/gm"),
        )

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
      let result =
        content->Js.String2.match_(
          %re("/\s*(\"[^\"]*\"|\`[^\`]*\`|\'[^\']*\'|\([^)]*\)|\[[^\]]*\]|\{[^\}]*\}|[^,]+)/gm"),
        )
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
  | str if str->isFunction("AccessProviders") => AccessProviders()
  | str if str->isFunction("Collections") => {
      let content = str->getContent("Collections")
      switch content {
      | "" => Collections(None)
      | content => Collections(Some(content->make))
      }
    }

  // other
  | str => raise(SyntaxError("other " ++ str))
  }
}
