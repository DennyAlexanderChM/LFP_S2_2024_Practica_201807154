module class_Inventory

    use class_Product ! Importamos la clase producto
    public
    integer :: numProducts = 0

    contains ! Funciones y subrutinas

    ! Subrutina para la lectuar de los archivos
    subroutine readFile()

        character (len = 100) :: linea
        
        integer estadoApertura, i

        ! Abrimos el archivo
        OPEN(UNIT=1, FILE='incial.inv', STATUS='OLD', ACTION='READ', IOSTAT= estadoApertura)

        ! Si la apertura del archivo no tiene errores
        if ( estadoApertura /= 0 ) then

            print *, "A ocurrido un error al abrir el archivo"

            stop

        end if
        
        ! Si la apertura del archico es correcta

        i = 0
            
        DO  ! Suponemos que el archivo tiene como máximo 4 líneas

            READ(1, '(A)', IOSTAT=estadoApertura) linea

            if (estadoApertura /= 0) exit

            i = i + 1

            print *, 'Línea ', i, ': ', trim(linea)
            
        END DO

            ! Cerramos el archivo
        close(1)
  
    end subroutine readFile

    subroutine addProduct(array,  name, amount, price, location)

        type(Product), allocatable, intent(inout) :: array(:)
        type(Product), allocatable :: temp(:)
        type(product) :: newProduct
        character(len=100), intent(in) ::  name, location
        integer, intent(in)  :: amount
        real, intent(in) :: price

        newProduct%name = name
        newProduct%amount = amount
        newProduct%price = price
        newProduct%location = location

        numProducts = numProducts + 1

        if ( allocated(array) ) then

            allocate(temp(numProducts-1))

            temp = array

            deallocate(array)

            allocate(array(numProducts))

            array(1:num_personas-1)= temp

            deallocate(temp)
        else
            allocate(array(numProducts))
        end if

        array(numProducts) = newProduct
        
    end subroutine addProduct
    
end module class_Inventory