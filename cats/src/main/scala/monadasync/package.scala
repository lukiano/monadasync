package object monadasync {
  type Callback[A] = (A => Unit) => Unit
}
