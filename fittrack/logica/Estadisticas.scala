package fittrack.logica

import fittrack.modelo.*
import fittrack.util.*

sealed trait Tendencia
case class Mejora(porcentaje: Double) extends Tendencia
case class Descenso(porcentaje: Double) extends Tendencia
case object Estable extends Tendencia

object Tendencia {
    def mensaje(tendencia: Tendencia): String =
        tendencia match
            case Mejora(p) =>
                s"Tu volumen subió un ${p.round}% respecto a tu sesión anterior de la misma rutina. ¡Buen trabajo!"
            case Descenso(p) =>
                s"Tu volumen bajó un ${p.round}% respecto a tu sesión anterior de la misma rutina."
            case Estable =>
                "Mantuviste el mismo volumen que en tu sesión anterior."
}

case class ResumenEstadistico(
    totalSesiones: Int,
    volumenTotal: Double,
    seriesTotales: Int,
    ejercicioMasEntrenado: Option[(Ejercicio, Int)],
    grupoMasTrabajado: Option[(GrupoMuscular, Int)],
    sesionMayorVolumen: Option[Sesion],
    tendencia: Option[Tendencia]
)

object Estadisticas {

    private def registros(historial: List[Sesion]): List[RegistroEjercicio] =
        historial.foldLeft(List.empty[RegistroEjercicio])((lista, sesion) =>
            lista ++ sesion.registros
        )

    def volumenTotal(historial: List[Sesion]): Double =
        historial.foldLeft(0.0)((total, sesion) => total + sesion.volumenTotal)

    def seriesTotales(historial: List[Sesion]): Int =
        historial.foldLeft(0)((total, sesion) => total + sesion.totalSeries)

    private def contarPor[A](
        historial: List[Sesion],
        clave: Ejercicio => A
    ): List[(A, Int)] = {
        val datos = registros(historial)
        val claves = Listas.sinRepetidos(datos.map(registro => clave(registro.ejercicio)))

        claves.map(valor => {
            val coincidencias = datos.filter(registro => clave(registro.ejercicio) == valor)
            val cantidad = coincidencias.foldLeft(0)((total, registro) =>
                total + registro.series.length
            )
            (valor, cantidad)
        })
    }

    def ejercicioMasEntrenado(historial: List[Sesion]): Option[(Ejercicio, Int)] =
        Listas.maximoPor(contarPor(historial, (ejercicio: Ejercicio) => ejercicio))(_._2.toDouble)

    def grupoMasTrabajado(historial: List[Sesion]): Option[(GrupoMuscular, Int)] =
        Listas.maximoPor(contarPor(historial, (ejercicio: Ejercicio) => ejercicio.grupo))(_._2.toDouble)

    def sesionMayorVolumen(historial: List[Sesion]): Option[Sesion] =
        Listas.maximoPor(historial)(_.volumenTotal)

    def recordPersonal(historial: List[Sesion], ejercicio: Ejercicio): Double =
        registros(historial)
            .filter(_.ejercicio == ejercicio)
            .foldLeft(0.0)((mayor, registro) => math.max(mayor, registro.mejorMarca))

    def recordsPersonales(historial: List[Sesion]): List[(Ejercicio, Double)] = {
        val ejercicios = Listas.sinRepetidos(registros(historial).map(_.ejercicio))
        ejercicios.map(ejercicio => (ejercicio, recordPersonal(historial, ejercicio)))
    }

    def evolucionVolumen(historial: List[Sesion]): List[(Fecha, Double)] =
        historial.map(sesion => (sesion.fecha, sesion.volumenTotal))

    private def evolucionPorEjercicio(
        historial: List[Sesion],
        ejercicio: Ejercicio
    )(calcular: List[RegistroEjercicio] => Double): List[(Fecha, Double)] =
        historial.foldLeft(List.empty[(Fecha, Double)])((resultado, sesion) => {
            val encontrados = sesion.registros.filter(_.ejercicio == ejercicio)
            if encontrados.isEmpty then resultado
            else resultado :+ (sesion.fecha, calcular(encontrados))
        })

    def progresoEjercicio(
        historial: List[Sesion],
        ejercicio: Ejercicio
    ): (String, List[(Fecha, Double)]) =
        ejercicio.medicion match
            case RepeticionesConPeso =>
                (
                    "Mejor peso por sesión (kg)",
                    evolucionPorEjercicio(historial, ejercicio)(
                        registros => registros.foldLeft(0.0)((mayor, r) => math.max(mayor, r.mejorPeso))
                    )
                )
            case SoloRepeticiones =>
                (
                    "Repeticiones por sesión",
                    evolucionPorEjercicio(historial, ejercicio)(
                        registros => registros.foldLeft(0)((total, r) => total + r.repeticiones).toDouble
                    )
                )
            case PorTiempo =>
                (
                    "Tiempo por sesión (segundos)",
                    evolucionPorEjercicio(historial, ejercicio)(
                        registros => registros.foldLeft(0)((total, r) => total + r.segundos).toDouble
                    )
                )
            case PorDistancia | DistanciaYTiempo =>
                (
                    "Distancia por sesión (km)",
                    evolucionPorEjercicio(historial, ejercicio)(
                        registros => registros.foldLeft(0.0)((total, r) => total + r.kilometros)
                    )
                )

    private def clasificar(anterior: Double, actual: Double): Option[Tendencia] =
        if anterior == 0.0 then None
        else if anterior == actual then Some(Estable)
        else {
            val porcentaje = (actual - anterior) / anterior * 100.0
            if porcentaje > 0 then Some(Mejora(porcentaje))
            else Some(Descenso(-porcentaje))
        }

    def tendencia(historial: List[Sesion]): Option[Tendencia] = {
        val ultimaSesion = historial.foldLeft(Option.empty[Sesion])((_, sesion) => Some(sesion))

        ultimaSesion match
            case None => None
            case Some(ultima) =>
                val sesiones = historial.filter(_.nombreRutina == ultima.nombreRutina)
                val dosUltimas = sesiones.foldLeft(
                    (Option.empty[Sesion], Option.empty[Sesion])
                )((par, sesion) => (par._2, Some(sesion)))

                dosUltimas match
                    case (Some(anterior), Some(actual)) =>
                        clasificar(anterior.volumenTotal, actual.volumenTotal)
                    case _ => None
    }

    def generarResumen(historial: List[Sesion]): ResumenEstadistico =
        ResumenEstadistico(
            historial.length,
            volumenTotal(historial),
            seriesTotales(historial),
            ejercicioMasEntrenado(historial),
            grupoMasTrabajado(historial),
            sesionMayorVolumen(historial),
            tendencia(historial)
        )
}
