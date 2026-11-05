package fittrack.modelo

import fittrack.util.*

case class Fecha private (
    anio: Int,
    mes: Int,
    dia: Int
) {
    def valor: Int = anio * 10000 + mes * 100 + dia


    def descripcion: String = f"$dia%02d/$mes%02d/$anio"

    def corta: String = s"$dia/$mes"
}

object Fecha {

    val referencia: Fecha = new Fecha(2026, 1, 1)

    def diasDelMes(anio: Int, mes: Int): Int = {
        mes match
            case 2 => 28
            case 4 | 6 | 9 | 11 => 30
            case _ => 31
    }

    def crear(anio: Int, mes: Int, dia: Int): Resultado[Fecha] =
        if anio < 2000 || anio > 2100 then Fallo("El año debe estar entre 2000 y 2100.")
        else if mes < 1 || mes > 12 then Fallo("El mes debe estar entre 1 y 12.")
        else if dia < 1 || dia > diasDelMes(anio, mes) then Fallo(s"Ese mes tiene ${diasDelMes(anio, mes)} días como máximo.")
        else Exito(new Fecha(anio, mes, dia))
}
