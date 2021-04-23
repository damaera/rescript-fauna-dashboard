type opts = {
  disabled: option<bool>,
  indentation: option<int>,
}

@module("use-editable")
external useEditable: (
  React.ref<Js.Nullable.t<Dom.element>>,
  (string => string) => unit,
  option<opts>,
) => unit = "useEditable"

@react.component
let make = (~code, ~setCode, ~language="jsx") => {
  let editorRef = React.useRef(Js.Nullable.null)

  useEditable(
    editorRef,
    setCode,
    Some({
      disabled: Some(false),
      indentation: Some(2),
    }),
  )

  <PrismRenderer.Highlight
    \"Prism"={PrismRenderer.defaultProps["Prism"]} theme={PrismRenderer.themeGithub} code language>
    {props =>
      <pre className={props.className} style={props.style} ref={ReactDOM.Ref.domRef(editorRef)}>
        {
          let arrLength = props.tokens->Js.Array2.length

          props.tokens
          ->Js.Array2.mapi((line, i) => {
            // Js.log(i)
            <React.Fragment key={i->string_of_int}>
              {line
              ->Js.Array2.filter(token => token.empty != Some(true))
              ->Js.Array2.mapi((token, key) => {
                let prop = props.getTokenProps({"token": token, "key": key})
                Js.log(prop)
                <span className={prop["className"]} key={key->string_of_int} style={prop["style"]}>
                  {prop["children"]}
                </span>
              })
              ->React.array}
              {i < arrLength - 1 ? "\n"->React.string : React.null}
            </React.Fragment>
          })
          ->React.array
        }
      </pre>}
  </PrismRenderer.Highlight>
}
