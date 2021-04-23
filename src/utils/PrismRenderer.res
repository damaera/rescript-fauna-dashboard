type tokenT = {
  types: array<string>,
  content: string,
  empty: option<bool>,
}

type renderPropsT<'style, 'lineProps, 'tokenProps> = {
  tokens: array<array<tokenT>>,
  className: string,
  style: 'style,
  getLineProps: 'lineProps,
  getTokenProps: 'tokenProps,
}

@module("prism-react-renderer") external prism: 'a = "Prism"

@module("prism-react-renderer") external defaultProps: 'a = "defaultProps"

module Highlight = {
  @module("prism-react-renderer") @react.component
  external make: (
    ~\"Prism": 'prism,
    ~theme: 'theme=?,
    ~code: string,
    ~language: string,
    ~children: renderPropsT<ReactDOM.Style.t, 'a => 'b, 'c => 'd> => React.element,
  ) => React.element = "default"
}

@module("prism-react-renderer/themes/duotoneLight") external themeDuotoneLight: 'a = "default"
@module("prism-react-renderer/themes/github") external themeGithub: 'a = "default"