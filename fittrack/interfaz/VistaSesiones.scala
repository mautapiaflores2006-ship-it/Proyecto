package fittrack.interfaz

import java.awt.{BorderLayout, CardLayout, Dimension, FlowLayout, GridLayout}
import javax.swing.*
import fittrack.logica.*
import fittrack.modelo.*
import fittrack.util.*

/**
 * Entrenar: sin sesión abierta se puede empezar una y revisar el historial;
 * con sesión abierta se van anotando las series y al final se guarda.
 */
class VistaSesiones(controlador: Controlador) extends Vista {

    val panel: JPanel = Componentes.lienzo(new BorderLayout())

    private val cartas = new CardLayout()
    private val zona = Componentes.lienzo(cartas)

    // --- Sin sesión abierta
    private val selectorRutina = Componentes.selector[Option[Rutina]](Nil)
    private val campoDia = Componentes.campoTexto(3)
    private val campoMes = Componentes.campoTexto(3)
    private val campoAnio = Componentes.campoTexto(5)
    private val avisoInicio = new Aviso()
    private val botonEmpezar = new BotonPlano("Empezar sesión", Primario)
    private val listaHistorial = new ListaOpciones[Sesion](true)
    private val detalleHistorial = Componentes.areaTexto()
    private val botonEliminarSesion = new BotonPlano("Eliminar esta sesión", Peligro)

    // --- Con sesión abierta
    private val etiquetaSesion = Componentes.etiqueta(" ", Tema.fuenteTitulo, Tema.tinta)
    private val etiquetaRutina = Componentes.etiqueta(" ", Tema.fuenteCuerpo, Tema.tintaSuave)
    private val selectorEjercicio = Componentes.selector[Ejercicio](Nil)
    private val campoReps = Componentes.campoTexto(6)
    private val campoPeso = Componentes.campoTexto(6)
    private val campoMinutos = Componentes.campoTexto(6)
    private val campoSegundos = Componentes.campoTexto(6)
    private val campoKilometros = Componentes.campoTexto(6)
    private val formRepeticiones = Componentes.formulario(List(("Repeticiones", campoReps)))
    private val formPeso = Componentes.formulario(List(("Peso (kg)", campoPeso)))
    private val formMinutos = Componentes.formulario(List(("Minutos", campoMinutos)))
    private val formSegundos = Componentes.formulario(List(("Segundos", campoSegundos)))
    private val formKilometros = Componentes.formulario(List(("Distancia (km)", campoKilometros)))
    private val zonaMedidas = Componentes.lienzo(new GridLayout(0, 2, 16, 0))
    private val etiquetaAyuda = Componentes.etiqueta(" ", Tema.fuentePequena, Tema.tintaSuave)
    private val avisoCurso = new Aviso()
    private val botonSerie = new BotonPlano("Añadir serie", Primario)
    private val botonDeshacer = new BotonPlano("Deshacer última serie", Secundario)
    private val botonGuardar = new BotonPlano("Guardar sesión", Primario)
    private val botonDescartar = new BotonPlano("Descartar", Peligro)
    private val resumenCurso = Componentes.areaTexto()

    construir()

    private def construir(): Unit = {
        val encabezado = Componentes.columna()
        encabezado.setBorder(Tema.margen(0, 0, 24, 0))
        encabezado.add(Componentes.izquierda(Componentes.etiqueta("Entrenar", Tema.fuenteDisplay, Tema.tinta)))
        encabezado.add(Box.createVerticalStrut(6))
        encabezado.add(Componentes.izquierda(Componentes.etiqueta(
            "Anota lo que haces serie por serie y revisa tus sesiones anteriores.", Tema.fuenteCuerpo, Tema.tintaSuave)))

        val (anio, mes, dia) = Componentes.hoy()
        campoDia.setText(dia.toString)
        campoMes.setText(mes.toString)
        campoAnio.setText(anio.toString)
        campoReps.setText("10")

        zona.add(construirLibre(), "libre")
        zona.add(construirCurso(), "curso")

        panel.setBorder(Tema.margen(40, 52, 40, 52))
        panel.add(encabezado, BorderLayout.NORTH)
        panel.add(zona, BorderLayout.CENTER)

        botonEmpezar.addActionListener(_ => iniciar())
        listaHistorial.alSeleccionar(() => mostrarDetalleHistorial())
        botonEliminarSesion.addActionListener(_ => eliminarSesion())
        selectorEjercicio.addActionListener(_ => mostrarCampos())
        botonSerie.addActionListener(_ => anadirSerie())
        botonDeshacer.addActionListener(_ => deshacer())
        botonGuardar.addActionListener(_ => guardar())
        botonDescartar.addActionListener(_ => descartar())
    }

    private def construirLibre(): JPanel = {
        val fecha = Componentes.lienzo(new FlowLayout(FlowLayout.LEFT, 10, 0))
        fecha.add(campoDia)
        fecha.add(campoMes)
        fecha.add(campoAnio)

        val izquierda = Componentes.columna()
        izquierda.add(Componentes.izquierda(Componentes.seccion("Empezar una sesión")))
        izquierda.add(Componentes.izquierda(Componentes.formulario(List(
            ("¿Qué vas a hacer?", selectorRutina)
        ))))
        val etiquetaFecha = Componentes.etiqueta("Fecha (día, mes y año)", Tema.fuentePequena, Tema.tintaSuave)
        etiquetaFecha.setBorder(Tema.margen(0, 0, 4, 0))
        izquierda.add(Componentes.izquierda(etiquetaFecha))
        izquierda.add(Componentes.izquierda(fecha))
        izquierda.add(Box.createVerticalStrut(20))
        izquierda.add(Componentes.izquierda(botonEmpezar))
        izquierda.add(Box.createVerticalStrut(12))
        izquierda.add(Componentes.izquierda(avisoInicio))

        val historial = Componentes.lienzo(new BorderLayout(0, 14))
        historial.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createMatteBorder(0, 1, 0, 0, Tema.linea),
            Tema.margen(0, 32, 0, 0)
        ))
        val titulo = Componentes.seccion("Historial")
        historial.add(titulo, BorderLayout.NORTH)

        val centro = Componentes.lienzo(new BorderLayout(0, 14))
        val cajaLista = Componentes.desplazable(listaHistorial.lista)
        cajaLista.setPreferredSize(new Dimension(0, 190))
        centro.add(cajaLista, BorderLayout.NORTH)
        centro.add(detalleHistorial, BorderLayout.CENTER)
        val pie = Componentes.lienzo(new FlowLayout(FlowLayout.RIGHT, 0, 0))
        pie.add(botonEliminarSesion)
        centro.add(pie, BorderLayout.SOUTH)
        historial.add(centro, BorderLayout.CENTER)

        val columnaIzquierda = Componentes.lienzo(new BorderLayout())
        columnaIzquierda.setPreferredSize(new Dimension(340, 0))
        columnaIzquierda.add(izquierda, BorderLayout.NORTH)

        val p = Componentes.lienzo(new BorderLayout(32, 0))
        p.add(columnaIzquierda, BorderLayout.WEST)
        p.add(historial, BorderLayout.CENTER)
        p
    }

    private def construirCurso(): JPanel = {
        val cabecera = Componentes.columna()
        cabecera.setBorder(Tema.margen(0, 0, 18, 0))
        cabecera.add(Componentes.izquierda(etiquetaSesion))
        cabecera.add(Box.createVerticalStrut(4))
        cabecera.add(Componentes.izquierda(etiquetaRutina))

        val izquierda = Componentes.columna()
        izquierda.add(Componentes.izquierda(cabecera))
        izquierda.add(Componentes.izquierda(Componentes.formulario(List(
            ("Ejercicio", selectorEjercicio)
        ))))
        izquierda.add(Componentes.izquierda(zonaMedidas))
        izquierda.add(Box.createVerticalStrut(6))
        izquierda.add(Componentes.izquierda(etiquetaAyuda))
        izquierda.add(Box.createVerticalStrut(20))
        val acciones = Componentes.lienzo(new FlowLayout(FlowLayout.LEFT, 10, 0))
        acciones.add(botonSerie)
        acciones.add(botonDeshacer)
        izquierda.add(Componentes.izquierda(acciones))
        izquierda.add(Box.createVerticalStrut(12))
        izquierda.add(Componentes.izquierda(avisoCurso))

        val derecha = Componentes.lienzo(new BorderLayout(0, 16))
        derecha.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createMatteBorder(0, 1, 0, 0, Tema.linea),
            Tema.margen(0, 32, 0, 0)
        ))
        derecha.add(Componentes.seccion("Lo que llevas hoy"), BorderLayout.NORTH)
        derecha.add(resumenCurso, BorderLayout.CENTER)
        val pie = Componentes.lienzo(new FlowLayout(FlowLayout.RIGHT, 12, 0))
        pie.add(botonDescartar)
        pie.add(botonGuardar)
        derecha.add(pie, BorderLayout.SOUTH)

        val columnaIzquierda = Componentes.lienzo(new BorderLayout())
        columnaIzquierda.setPreferredSize(new Dimension(380, 0))
        columnaIzquierda.add(izquierda, BorderLayout.NORTH)

        val p = Componentes.lienzo(new BorderLayout(32, 0))
        p.add(columnaIzquierda, BorderLayout.WEST)
        p.add(derecha, BorderLayout.CENTER)
        p
    }

    // ===== Acciones: sin sesión abierta =====

    private def iniciar(): Unit = {
        val datos = (campoDia.getText().trim().toIntOption, campoMes.getText().trim().toIntOption, campoAnio.getText().trim().toIntOption)
        datos match
            case (Some(dia), Some(mes), Some(anio)) =>
                val rutina = Componentes.valorOpcional(selectorRutina)
                controlador.intentar(estado => estado.iniciarSesion(anio, mes, dia, rutina)) match
                    case None => avisoInicio.limpiar()
                    case Some(error) => avisoInicio.mostrar(error, true)
            case _ => avisoInicio.mostrar("Escribe el día, el mes y el año con números.", true)
    }

    private def mostrarDetalleHistorial(): Unit = {
        val texto = listaHistorial.seleccionado match
            case Some(sesion) => Textos.sesion(sesion)
            case None => "Todavía no hay sesiones guardadas."
        Componentes.textoDe(detalleHistorial).setText(texto)
        Componentes.textoDe(detalleHistorial).setCaretPosition(0)
    }

    private def eliminarSesion(): Unit = {
        listaHistorial.seleccionado match
            case None => avisoInicio.mostrar("Elige una sesión del historial.", true)
            case Some(sesion) =>
                if Componentes.confirmar(panel, s"¿Eliminar la sesión del ${sesion.fecha.descripcion}?") then
                    controlador.cambiar(estado => estado.eliminarSesion(sesion))
    }

    // ===== Acciones: con sesión abierta =====

    private def camposDe(medicion: Medicion): List[JPanel] = {
        medicion match
            case RepeticionesConPeso => List(formRepeticiones, formPeso)
            case SoloRepeticiones => List(formRepeticiones)
            case PorTiempo => List(formMinutos, formSegundos)
            case PorDistancia => List(formKilometros)
            case DistanciaYTiempo => List(formKilometros, formMinutos, formSegundos)
    }

    private def mostrarCampos(): Unit = {
        val medicion = Componentes.valorSeleccionado(selectorEjercicio).map(e => e.medicion)
        val ayuda = medicion.map(m => Textos.ayudaMedicion(m)).getOrElse(" ")
        zonaMedidas.removeAll()
        medicion.foreach(m => camposDe(m).foreach(c => zonaMedidas.add(c)))
        etiquetaAyuda.setText(ayuda)
        zonaMedidas.revalidate()
        zonaMedidas.repaint()
    }

    private def anadirSerie(): Unit = {
        Componentes.valorSeleccionado(selectorEjercicio) match
            case None => avisoCurso.mostrar("Elige un ejercicio.", true)
            case Some(ejercicio) =>
                val medidas = Textos.leerMedidas(
                    ejercicio.medicion,
                    campoReps.getText(),
                    campoPeso.getText(),
                    campoMinutos.getText(),
                    campoSegundos.getText(),
                    campoKilometros.getText()
                )
                medidas match
                    case Fallo(error) => avisoCurso.mostrar(error, true)
                    case Exito(m) =>
                        controlador.intentar(estado => estado.agregarSerie(ejercicio, m)) match
                            case None => avisoCurso.mostrar("Serie añadida.", false)
                            case Some(error) => avisoCurso.mostrar(error, true)
    }

    private def deshacer(): Unit = {
        Componentes.valorSeleccionado(selectorEjercicio) match
            case None => avisoCurso.mostrar("Elige un ejercicio.", true)
            case Some(ejercicio) =>
                controlador.cambiar(estado => estado.quitarUltimaSerie(ejercicio))
                avisoCurso.mostrar("Última serie deshecha.", false)
    }

    private def guardar(): Unit = {
        controlador.intentar(estado => estado.guardarSesion) match
            case None => avisoCurso.limpiar()
            case Some(error) => avisoCurso.mostrar(error, true)
    }

    private def descartar(): Unit = {
        if Componentes.confirmar(panel, "¿Descartar esta sesión? Se perderá lo que anotaste.") then {
            controlador.cambiar(estado => estado.descartarSesion)
            avisoCurso.limpiar()
        }
    }

    // ===== Dibujo =====

    private def actualizarCurso(estado: AppEstado, sesion: Sesion): Unit = {
        etiquetaSesion.setText(s"Sesión del ${sesion.fecha.descripcion}")
        etiquetaRutina.setText(Textos.nombreSesion(sesion))
        val enSesion = sesion.registros.map(r => r.ejercicio)
        val disponibles = sesion.nombreRutina match
            case Some(_) => enSesion
            case None => enSesion ::: estado.catalogo.filter(e => !enSesion.contains(e))
        Componentes.llenarSelector(selectorEjercicio, disponibles.map(e => new Opcion(e, e.nombre)))
        mostrarCampos()
        Componentes.textoDe(resumenCurso).setText(Textos.sesion(sesion))
        Componentes.textoDe(resumenCurso).setCaretPosition(0)
    }

    override def refrescar(): Unit = {
        val estado = controlador.estado
        val sinRutina = new Opcion[Option[Rutina]](None, "Sesión libre (sin rutina)")
        val conRutina = estado.rutinas.map(r => new Opcion[Option[Rutina]](Some(r), r.nombre))
        Componentes.llenarSelector(selectorRutina, sinRutina :: conRutina)
        listaHistorial.mostrar(estado.historial.reverse.map(s => new Opcion(s, Textos.resumenCorto(s))))
        mostrarDetalleHistorial()
        estado.sesionEnCurso match
            case None =>
                cartas.show(zona, "libre")
            case Some(sesion) =>
                actualizarCurso(estado, sesion)
                cartas.show(zona, "curso")
    }
}
