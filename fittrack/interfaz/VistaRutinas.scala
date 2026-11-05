package fittrack.interfaz

import java.awt.{BorderLayout, Dimension, FlowLayout}
import javax.swing.*
import fittrack.logica.*
import fittrack.modelo.*
import fittrack.util.*

/** Rutinas: crearlas, armarlas con ejercicios del catálogo y empezar a entrenar con una. */
class VistaRutinas(controlador: Controlador) extends Vista {

    val panel: JPanel = Componentes.lienzo(new BorderLayout())

    private val listaRutinas = new ListaOpciones[Rutina](true)
    private val listaEjercicios = new ListaOpciones[Ejercicio](false)
    private val etiquetaNombre = Componentes.etiqueta(" ", Tema.fuenteTitulo, Tema.tinta)
    private val selectorAgregar = Componentes.selector[Ejercicio](Nil)

    private val campoNombre = Componentes.campoTexto(16)
    private val avisoNueva = new Aviso()
    private val avisoDetalle = new Aviso()

    private val botonCrear = new BotonPlano("Crear rutina", Primario)
    private val botonAgregar = new BotonPlano("Añadir", Secundario)
    private val botonQuitar = new BotonPlano("Quitar de la rutina", Secundario)
    private val botonEntrenar = new BotonPlano("Entrenar con esta rutina", Primario)
    private val botonEliminar = new BotonPlano("Eliminar rutina", Peligro)

    construir()

    // Construye y conecta los componentes de la vista de rutinas.
    private def construir(): Unit = {
        val encabezado = Componentes.columna()
        encabezado.setBorder(Tema.margen(0, 0, 24, 0))
        encabezado.add(Componentes.izquierda(Componentes.etiqueta("Rutinas", Tema.fuenteDisplay, Tema.tinta)))
        encabezado.add(Box.createVerticalStrut(6))
        encabezado.add(Componentes.izquierda(Componentes.etiqueta(
            "Tus planes de entrenamiento, con los ejercicios en el orden en que los haces.", Tema.fuenteCuerpo, Tema.tintaSuave)))

        // Columna izquierda: lista de rutinas + nueva rutina
        val izquierda = Componentes.lienzo(new BorderLayout(0, 24))
        izquierda.setPreferredSize(new Dimension(360, 0))
        izquierda.add(Componentes.desplazable(listaRutinas.lista), BorderLayout.CENTER)

        val nueva = Componentes.columna()
        nueva.add(Componentes.izquierda(Componentes.seccion("Una rutina nueva")))
        nueva.add(Componentes.izquierda(Componentes.formulario(List(
            ("Nombre", campoNombre)
        ))))
        nueva.add(Componentes.izquierda(botonCrear))
        nueva.add(Box.createVerticalStrut(10))
        nueva.add(Componentes.izquierda(avisoNueva))
        izquierda.add(nueva, BorderLayout.SOUTH)

        // Columna derecha: detalle de la rutina elegida
        val titulo = Componentes.columna()
        titulo.add(Componentes.izquierda(etiquetaNombre))
        titulo.setBorder(Tema.margen(0, 0, 16, 0))

        val filaAgregar = Componentes.lienzo(new FlowLayout(FlowLayout.LEFT, 12, 0))
        filaAgregar.add(selectorAgregar)
        filaAgregar.add(botonAgregar)
        filaAgregar.add(botonQuitar)

        val acciones = Componentes.lienzo(new FlowLayout(FlowLayout.LEFT, 12, 0))
        acciones.add(botonEntrenar)
        acciones.add(botonEliminar)

        val pie = Componentes.columna()
        pie.setBorder(Tema.margen(16, 0, 0, 0))
        pie.add(Componentes.izquierda(filaAgregar))
        pie.add(Box.createVerticalStrut(18))
        pie.add(Componentes.izquierda(acciones))
        pie.add(Box.createVerticalStrut(10))
        pie.add(Componentes.izquierda(avisoDetalle))

        val derecha = Componentes.lienzo(new BorderLayout())
        derecha.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createMatteBorder(0, 1, 0, 0, Tema.linea),
            Tema.margen(0, 32, 0, 0)
        ))
        derecha.add(titulo, BorderLayout.NORTH)
        derecha.add(Componentes.desplazable(listaEjercicios.lista), BorderLayout.CENTER)
        derecha.add(pie, BorderLayout.SOUTH)

        val cuerpo = Componentes.lienzo(new BorderLayout(32, 0))
        cuerpo.add(izquierda, BorderLayout.WEST)
        cuerpo.add(derecha, BorderLayout.CENTER)

        panel.setBorder(Tema.margen(40, 52, 40, 52))
        panel.add(encabezado, BorderLayout.NORTH)
        panel.add(cuerpo, BorderLayout.CENTER)

        listaRutinas.alSeleccionar(() => actualizarDetalle())
        botonCrear.addActionListener(_ => crear())
        botonAgregar.addActionListener(_ => agregar())
        botonQuitar.addActionListener(_ => quitar())
        botonEntrenar.addActionListener(_ => entrenar())
        botonEliminar.addActionListener(_ => eliminar())
    }

    // Convierte ejercicios en opciones numeradas para mostrarlos.
    private def numerar(ejercicios: List[Ejercicio], n: Int): List[Opcion[Ejercicio]] = {
        ejercicios match
            case Nil => Nil
            case cabeza :: cola => new Opcion(cabeza, s"$n.  ${cabeza.nombre}") :: numerar(cola, n + 1)
    }

    // Genera el texto breve que identifica una rutina en la lista.
    private def textoRutina(r: Rutina): String =
        s"${r.nombre}  ·  ${r.cantidadEjercicios} ejercicios"

    // Activa o desactiva las acciones según exista una rutina seleccionada.
    private def habilitar(activo: Boolean): Unit = {
        botonAgregar.setEnabled(activo)
        botonQuitar.setEnabled(activo)
        botonEntrenar.setEnabled(activo)
        botonEliminar.setEnabled(activo)
        selectorAgregar.setEnabled(activo)
    }

    // Actualiza el panel derecho según la rutina seleccionada.
    private def actualizarDetalle(): Unit = {
        listaRutinas.seleccionado match
            case None =>
                etiquetaNombre.setText("Elige o crea una rutina")
                listaEjercicios.mostrar(Nil)
                Componentes.llenarSelector(selectorAgregar, Nil)
                habilitar(false)
            case Some(rutina) =>
                etiquetaNombre.setText(rutina.nombre)
                listaEjercicios.mostrar(numerar(rutina.ejercicios, 1))
                val disponibles = controlador.estado.catalogo.filter(e => !rutina.contiene(e))
                Componentes.llenarSelector(selectorAgregar, disponibles.map(e => new Opcion(e, e.nombre)))
                habilitar(true)
    }

    // Crea una rutina usando el nombre escrito.
    private def crear(): Unit = {
        controlador.intentar(estado => estado.crearRutina(campoNombre.getText())) match
            case None =>
                campoNombre.setText("")
                listaRutinas.seleccionarUltimo()
                avisoNueva.mostrar("Rutina creada. Ahora añádele ejercicios.", false)
            case Some(error) =>
                avisoNueva.mostrar(error, true)
    }

    // Agrega el ejercicio seleccionado a la rutina actual.
    private def agregar(): Unit = {
        (listaRutinas.seleccionado, Componentes.valorSeleccionado(selectorAgregar)) match
            case (Some(rutina), Some(ejercicio)) =>
                controlador.cambiar(estado => estado.agregarEjercicioARutina(rutina.nombre, ejercicio))
                avisoDetalle.limpiar()
            case _ => avisoDetalle.mostrar("Elige un ejercicio para añadir.", true)
    }

    // Quita el ejercicio seleccionado de la rutina actual.
    private def quitar(): Unit = {
        (listaRutinas.seleccionado, listaEjercicios.seleccionado) match
            case (Some(rutina), Some(ejercicio)) =>
                controlador.cambiar(estado => estado.quitarEjercicioDeRutina(rutina.nombre, ejercicio))
                avisoDetalle.limpiar()
            case _ => avisoDetalle.mostrar("Elige primero un ejercicio de la lista.", true)
    }

    // Inicia una sesión con la rutina seleccionada.
    private def entrenar(): Unit = {
        listaRutinas.seleccionado match
            case None => avisoDetalle.mostrar("Elige una rutina.", true)
            case Some(rutina) =>
                val (anio, mes, dia) = Componentes.hoy()
                controlador.intentar(estado => estado.iniciarSesion(anio, mes, dia, Some(rutina))) match
                    case None =>
                        avisoDetalle.limpiar()
                        controlador.irA("sesiones")
                    case Some(error) =>
                        avisoDetalle.mostrar(error, true)
    }

    // Solicita confirmación y elimina la rutina seleccionada.
    private def eliminar(): Unit = {
        listaRutinas.seleccionado match
            case None => avisoDetalle.mostrar("Elige una rutina.", true)
            case Some(rutina) =>
                if Componentes.confirmar(panel, s"¿Eliminar la rutina «${rutina.nombre}»?") then {
                    controlador.cambiar(estado => estado.eliminarRutina(rutina.nombre))
                    avisoDetalle.limpiar()
                }
    }

    // Recarga la lista de rutinas y actualiza su detalle.
    override def refrescar(): Unit = {
        listaRutinas.mostrar(controlador.estado.rutinas.map(r => new Opcion(r, textoRutina(r))))
        actualizarDetalle()
    }
}
