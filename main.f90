program main
    use class_Product

    implicit none

    ! Define variables
    ! Variable para la guardar la opción seleccionada
    character(:), allocatable :: name
    integer:: optionSelect = 0
    type(Product) :: myProduct

    !ciclo hasta que la opción seleccionada sea 2 (finalizar programa)
    do while (optionSelect /= 4)
        ! Impresión del menú en pantalla
        print *, "-------------------------------------------------"
        print *, "Practica 1 - Lenguajes formales y de programacion"
        print *, "-------------------------------------------------"
        print *, "# Sistema de inventario"
        print *, " 1. Cargar Inventario Inicial"
        print *, " 2. Cargar Instrucciones de movimientos"
        print *, " 3. Crear Informe de inventario"
        print *, " 4. Salir"
        print *, "Ingrese una opcion:"

        ! Lectura de la opcion desde el teclado
        read(*,*) optionSelect
        ! Select para dirigir a la opción ingresada
        SELECT CASE (optionSelect)
        CASE (1)
            ! Llamamos al caso 2 que imprime el siguiente mensaje
        CASE (2)
            print *, "Segunda Opcion"
        CASE (3)
            print *, "Tercera Opcion"
        CASE (4)
            WRITE(*,*)  'Finalizando...'
            ! Mansaje si se ingresa una opción
        CASE DEFAULT
            WRITE(*,*)  "Ingrese una opción correcta"
        END SELECT
    end do
    
end program main