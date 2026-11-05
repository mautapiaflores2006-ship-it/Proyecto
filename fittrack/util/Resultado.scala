package fittrack.util

sealed trait Resultado[A] {

    def map[B](f: A => B): Resultado[B] =
        this match
            case Exito(valor) => Exito(f(valor))
            case Fallo(mensaje) => Fallo(mensaje)

    def flatMap[B](f: A => Resultado[B]): Resultado[B] =
        this match
            case Exito(valor) => f(valor)
            case Fallo(mensaje) => Fallo(mensaje)

    def obtenerOSino(porDefecto: A): A =
        this match
            case Exito(valor) => valor
            case Fallo(_) => porDefecto
}

case class Exito[A](valor: A) extends Resultado[A]
case class Fallo[A](mensaje: String) extends Resultado[A]

object Resultado {

    def exitosos[A](lista: List[Resultado[A]]): List[A] =
        lista.foldLeft(List.empty[A])((resultado, elemento) =>
            elemento match
                case Exito(valor) => resultado :+ valor
                case Fallo(_) => resultado
        )
}
