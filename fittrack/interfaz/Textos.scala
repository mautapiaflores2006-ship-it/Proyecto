package fittrack.interfaz

import fittrack.modelo.*
import fittrack.util.*

/** Convierte datos del modelo en texto legible (la lógica no sabe nada de cómo se muestra). */
object Textos {

    def nombreSesion(sesion: Sesion): String = {
        sesion.nombreRutina match
            case Some(nombre) => nombre
            case None => "Sesión libre"
    }

    def serie(s: Serie): String = {
        s match
            case SerieConPeso(r, p) => s"$r repeticiones con ${Formato.numero(p)} kg"
            case SerieDeRepeticiones(r) => s"$r repeticiones"
            case SerieDeTiempo(seg) => Formato.tiempo(seg)
            case SerieDeDistancia(k) => Formato.distancia(k)
            case SerieDeDistanciaYTiempo(k, seg) => s"${Formato.distancia(k)} en ${Formato.tiempo(seg)}"
    }

    def marca(ejercicio: Ejercicio, valor: Double): String = {
        ejercicio.medicion match
            case RepeticionesConPeso => s"${Formato.numero(valor)} kg"
            case SoloRepeticiones => s"${valor.toInt} repeticiones"
            case PorTiempo => Formato.tiempo(valor.toInt)
            case PorDistancia | DistanciaYTiempo => Formato.distancia(valor)
    }

    def ayudaMedicion(medicion: Medicion): String = {
        medicion match
            case RepeticionesConPeso => "Este ejercicio se mide con repeticiones y peso en kilos."
            case SoloRepeticiones => "Este ejercicio se mide solo por repeticiones."
            case PorTiempo => "Este ejercicio se mide solo por tiempo."
            case PorDistancia => "Este ejercicio se mide solo por distancia."
            case DistanciaYTiempo => "Este ejercicio se mide por distancia y tiempo."
    }

    private def lineasSeries(series: List[Serie], n: Int): List[String] = {
        series match
            case Nil => Nil
            case cabeza :: cola => s"      Serie $n:  ${serie(cabeza)}" :: lineasSeries(cola, n + 1)
    }

    def registro(r: RegistroEjercicio): String = {
        val lineas =
            if r.series.isEmpty then List("      (todavía sin series)")
            else lineasSeries(r.series, 1)
        (r.ejercicio.nombre :: lineas).mkString("\n")
    }

    private def totales(s: Sesion): String = {
        val partes = List(
            (s.volumenTotal > 0.0, s"Volumen: ${Formato.numero(s.volumenTotal)} kg"),
            (true, s"Series: ${s.totalSeries}"),
            (s.repeticionesTotales > 0, s"Repeticiones: ${s.repeticionesTotales}"),
            (s.tiempoTotal > 0, s"Tiempo: ${Formato.tiempo(s.tiempoTotal)}"),
            (s.distanciaTotal > 0.0, s"Distancia: ${Formato.distancia(s.distanciaTotal)}")
        )
        partes.filter(p => p._1).map(p => p._2).mkString("   ·   ")
    }

    /** Detalle completo de una sesión. */
    def sesion(s: Sesion): String = {
        val cuerpo =
            if s.registros.isEmpty then "Todavía no hay ejercicios en esta sesión."
            else s.registros.map(r => registro(r)).mkString("\n\n")
        s"${s.fecha.descripcion}  —  ${nombreSesion(s)}\n\n$cuerpo\n\n${totales(s)}"
    }

    /** Una línea para el historial. */
    def resumenCorto(s: Sesion): String = {
        val esfuerzo =
            if s.volumenTotal > 0.0 then s"${Formato.numero(s.volumenTotal)} kg"
            else if s.repeticionesTotales > 0 then s"${s.repeticionesTotales} repeticiones"
            else if s.tiempoTotal > 0 then Formato.tiempo(s.tiempoTotal)
            else Formato.distancia(s.distanciaTotal)
        s"${s.fecha.descripcion}  ·  ${nombreSesion(s)}  ·  $esfuerzo"
    }

    private def entero(texto: String, error: String): Resultado[Int] = {
        texto.trim().toIntOption match
            case Some(n) => Exito(n)
            case None => Fallo(error)
    }

    private def decimal(texto: String, error: String): Resultado[Double] = {
        texto.trim().replace(',', '.').toDoubleOption match
            case Some(d) => Exito(d)
            case None => Fallo(error)
    }

    private def enteroOCero(texto: String, error: String): Resultado[Int] =
        if texto.trim().length() == 0 then Exito(0) else entero(texto, error)

    private def leerTiempo(minutos: String, segundos: String): Resultado[Int] = {
        val error = "Escribe el tiempo con números: minutos y segundos."
        enteroOCero(minutos, error).flatMap(m => enteroOCero(segundos, error).map(s => m * 60 + s))
    }

    def leerMedidas(
        medicion: Medicion,
        repeticiones: String,
        peso: String,
        minutos: String,
        segundos: String,
        kilometros: String
    ): Resultado[Medidas] = {
        val errorRepeticiones = "Escribe las repeticiones con un número, por ejemplo 10."
        val errorPeso = "Escribe el peso en kilos, por ejemplo 40 o 12,5."
        val errorDistancia = "Escribe la distancia en kilómetros, por ejemplo 2,5."
        medicion match
            case RepeticionesConPeso =>
                entero(repeticiones, errorRepeticiones).flatMap(r =>
                    decimal(peso, errorPeso).map(p => Medidas.conPeso(r, p)))
            case SoloRepeticiones =>
                entero(repeticiones, errorRepeticiones).map(r => Medidas.repeticiones(r))
            case PorTiempo =>
                leerTiempo(minutos, segundos).map(s => Medidas.tiempo(s))
            case PorDistancia =>
                decimal(kilometros, errorDistancia).map(k => Medidas.distancia(k))
            case DistanciaYTiempo =>
                decimal(kilometros, errorDistancia).flatMap(k =>
                    leerTiempo(minutos, segundos).map(s => Medidas.distanciaYTiempo(k, s)))
    }
}
