program main
    use class_Product
    use class_Inventory

    implicit none

    ! Define variables
    type(Product), allocatable :: products(:) ! Almacenara cada uno de los productos del inventario

  
    type(Product) :: myProduct

    integer:: optionSelect, i

    optionSelect = 0
   
    ! character(:), allocatable :: name
    
    

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

        CASE (2)

            ! Asignar valores a los elementos del array
            do  
                print *, "Ingrese el nombre de la nueva persona (o 'salir' para terminar):"

                read *, myProduct%name

                if (trim(myProduct%name) == 'salir') exit

                print *, "Ingrese la cantidad ", myProduct%name , ":"
                read *, myProduct%amount
                print *, "Ingrese la precio ", myProduct%name , ":"
                read *, myProduct%price
                print *, "Ingrese la ubicacion ", myProduct%name , ":"
                read *, myProduct%location

                call addProduct(products, myProduct%name, myProduct%amount, myProduct%price, myProduct%location)

            end do

        CASE (3)
            ! Imprimir los valores
            print *, "Datos ingresados:"
            do i = 1, numProducts
                print *, 'Nombre:', products(i)%name, 'Cantidad:', products(i)%amount, 'Precio:', products(i)%price, 'Ubicacion:', products(i)%location
            end do
        CASE (4)
            WRITE(*,*)  'Finalizando...'
            ! Mansaje si se ingresa una opción
        CASE DEFAULT
            WRITE(*,*)  "Ingrese una opción correcta"
        END SELECT
    end do
    
end program main