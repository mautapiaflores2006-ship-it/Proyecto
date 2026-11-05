package fittrack.logica

import fittrack.modelo.*
import fittrack.util.*

object DatosIniciales {

    val pressBanca = Ejercicio("Press de banca", Pecho, Fuerza, RepeticionesConPeso)
    val flexiones = Ejercicio("Flexiones", Pecho, Fuerza, SoloRepeticiones)
    val remoConBarra = Ejercicio("Remo con barra", Espalda, Fuerza, RepeticionesConPeso)
    val pressMilitar = Ejercicio("Press militar", Hombros, Fuerza, RepeticionesConPeso)
    val curlBiceps = Ejercicio("Curl de bíceps", Brazos, Fuerza, RepeticionesConPeso)
    val sentadilla = Ejercicio("Sentadilla", Piernas, Fuerza, RepeticionesConPeso)
    val zancadas = Ejercicio("Zancadas", Piernas, Fuerza, RepeticionesConPeso)
    val elevacionTalones = Ejercicio("Elevación de talones", Piernas, Fuerza, SoloRepeticiones)
    val plancha = Ejercicio("Plancha", Core, Fuerza, PorTiempo)
    val caminata = Ejercicio("Caminata", CuerpoCompleto, Resistencia, PorDistancia)
    val circulosHombros = Ejercicio("Círculos de hombros", Hombros, Movilidad, SoloRepeticiones)
    val rotacionColumna = Ejercicio("Rotación de columna", Core, Movilidad, SoloRepeticiones)
    val estiramientoCadera = Ejercicio("Estiramiento de cadera", Piernas, Movilidad, PorTiempo)

    val catalogoInicial: List[Ejercicio] = List(
        pressBanca, flexiones, remoConBarra, pressMilitar, curlBiceps,
        sentadilla, zancadas, elevacionTalones, plancha, caminata,
        circulosHombros, rotacionColumna, estiramientoCadera
    )

    val cuerpoCompleto: Rutina = Rutina(
        "Cuerpo completo",
        List(sentadilla, pressBanca, remoConBarra, pressMilitar, plancha)
    )

    val movilidadSuave: Rutina = Rutina(
        "Movilidad suave",
        List(caminata, circulosHombros, rotacionColumna, estiramientoCadera)
    )

    val piernasYCore: Rutina = Rutina(
        "Piernas y core",
        List(sentadilla, zancadas, elevacionTalones, plancha)
    )

    val rutinasIniciales: List[Rutina] =
        List(cuerpoCompleto, movilidadSuave, piernasYCore)

    private def fecha(anio: Int, mes: Int, dia: Int): Fecha =
        Fecha.crear(anio, mes, dia).obtenerOSino(Fecha.referencia)

    private def conPeso(pares: List[(Int, Double)]): List[Serie] =
        Resultado.exitosos(pares.map(par =>
            Serie.crear(RepeticionesConPeso, Medidas.conPeso(par._1, par._2))
        ))

    private def repeticiones(cantidades: List[Int]): List[Serie] =
        Resultado.exitosos(cantidades.map(cantidad =>
            Serie.crear(SoloRepeticiones, Medidas.repeticiones(cantidad))
        ))

    private def tiempos(segundos: List[Int]): List[Serie] =
        Resultado.exitosos(segundos.map(cantidad =>
            Serie.crear(PorTiempo, Medidas.tiempo(cantidad))
        ))

    private def distancias(kilometros: List[Double]): List[Serie] =
        Resultado.exitosos(kilometros.map(cantidad =>
            Serie.crear(PorDistancia, Medidas.distancia(cantidad))
        ))

    private val sesion1 = Sesion(
        fecha(2026, 9, 14),
        List(
            RegistroEjercicio(pressBanca, conPeso(List((10, 50.0), (8, 55.0), (8, 55.0)))),
            RegistroEjercicio(sentadilla, conPeso(List((10, 60.0), (10, 60.0), (8, 65.0)))),
            RegistroEjercicio(remoConBarra, conPeso(List((10, 40.0), (10, 40.0)))),
            RegistroEjercicio(plancha, tiempos(List(30, 30)))
        ),
        Some("Cuerpo completo")
    )

    private val sesion2 = Sesion(
        fecha(2026, 9, 21),
        List(
            RegistroEjercicio(pressBanca, conPeso(List((10, 55.0), (8, 60.0), (8, 60.0)))),
            RegistroEjercicio(sentadilla, conPeso(List((10, 65.0), (10, 65.0), (8, 70.0)))),
            RegistroEjercicio(remoConBarra, conPeso(List((10, 42.5), (10, 42.5)))),
            RegistroEjercicio(plancha, tiempos(List(40, 40)))
        ),
        Some("Cuerpo completo")
    )

    private val sesion3 = Sesion(
        fecha(2026, 9, 28),
        List(
            RegistroEjercicio(caminata, distancias(List(1.5))),
            RegistroEjercicio(circulosHombros, repeticiones(List(15))),
            RegistroEjercicio(rotacionColumna, repeticiones(List(12))),
            RegistroEjercicio(estiramientoCadera, tiempos(List(30, 30)))
        ),
        Some("Movilidad suave")
    )

    val historialDemo: List[Sesion] = List(sesion1, sesion2, sesion3)
}
