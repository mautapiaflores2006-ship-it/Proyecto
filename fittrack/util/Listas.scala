package fittrack.util

object Listas {
    def sumarPor[A](lista: List[A])(valor: A => Double): Double =
        lista.foldLeft(0.0)((total, elemento) => total + valor(elemento))

    def maximoPor[A](lista: List[A])(valor: A => Double): Option[A] =
        lista match
            case Nil => None
            case primero :: resto =>
                Some(resto.foldLeft(primero)((mejor, actual) =>
                    if valor(actual) > valor(mejor) then actual else mejor))

    def sinRepetidos[A](lista: List[A]): List[A] =
        lista.foldLeft(List.empty[A])((resultado, elemento) =>
            if resultado.contains(elemento) then resultado else resultado :+ elemento)
}
