package fittrack.util

object Formato {

    def numero(valor: Double): String =
        if valor == valor.toLong.toDouble then valor.toLong.toString
        else f"$valor%.1f"

    def numeroFino(valor: Double): String = {
        val texto = f"$valor%.2f"
        if texto.endsWith("00") then texto.dropRight(3)
        else if texto.endsWith("0") then texto.dropRight(1)
        else texto
    }

    def tiempo(segundos: Int): String = {
        val partes = List(
            (segundos / 3600, "h"),
            ((segundos % 3600) / 60, "min"),
            (segundos % 60, "s")
        ).filter(parte => parte._1 > 0)
            .map(parte => s"${parte._1} ${parte._2}")

        if partes.isEmpty then "0 s" else partes.mkString(" ")
    }

    def distancia(kilometros: Double): String =
        s"${numeroFino(kilometros)} km"
}
