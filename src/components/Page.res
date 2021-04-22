%%raw(`import styles from "./Page.module.css"`)
@val external styles: {..} = "styles"

@react.component
let make = (~children) => {
  <div className={styles["page"]}> {children} </div>
}
