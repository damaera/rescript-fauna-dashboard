@react.component
let make = () => {
  let appStore = Store.store
  let appState = appStore.useStore()

  switch appState.screen {
  | Welcome => <WelcomeScreen />
  | Main => <MainScreen />
  // | _ => "Not implemeneted"->React.string
  }
}

let default = make
