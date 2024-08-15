module class_Analyzer

    use class_Product ! Importamos la clase producto
    public

contains

    subroutine analizarCadenaInicial(cadena, action, name, location, price, amount)
        character(len=200), intent(in) :: cadena
        character(len=100), intent(out) :: name, location, action
        real, intent(out) ::price
        integer, intent(out) :: amount
        integer :: i, estado
        character :: actual
        character(len=:), allocatable :: lexema
        character(len=101) :: cadenaNueva

        i = 0

        estado = 1
        
        actual = ""

        lexema = ""

        cadenaNueva = TRIM(cadena) // "#"

        do i = 1, LEN(cadenaNueva) ! obtiene la dimensian del array
            actual = cadenaNueva(i:i)
            
            if ( estado == 1  ) then
                
                if ( actual /= " " ) then

                    lexema = lexema // actual

                else    

                    action = lexema

                    lexema = ""

                    estado = 2
                    
                end if
            else if(estado == 2) then

                if (actual /= ";") then

                    lexema = lexema // actual
                    
                else    

                    name = lexema

                    estado = 3

                    lexema = ""

                end if
            else if(estado == 3) then

                if (actual /= ";") then

                    lexema = lexema // actual
                    
                else
                    read(lexema, *) amount
                    estado = 4
                    lexema = ""

                end if
            else if(estado == 4) then

                if (actual /= ";") then

                    lexema = lexema // actual
                    
                else
                    read(lexema, *) price

                    estado = 5
                    lexema = ""

                end if
            else if(estado == 5) then

                if (actual /= "#") then

                    lexema = lexema // actual
                    
                else
                    location = lexema
                    exit
                end if
            
            end if
                
        end do
    
    end subroutine analizarCadenaInicial

    subroutine analizarCadenaInstrucciones(cadena, action, name, location, amount)
        character(len=200), intent(in) :: cadena
        character(len=100), intent(out) :: name, location, action
        integer, intent(out) :: amount
        integer :: i, estado
        character :: actual
        character(len=:), allocatable :: lexema
        character(len=101) :: cadenaNueva

        i = 0

        estado = 1
        
        actual = ""

        lexema = ""

        cadenaNueva = TRIM(cadena) // "#"

        do i = 1, LEN(cadenaNueva) ! obtiene la dimensian del array
            actual = cadenaNueva(i:i)
            
            if ( estado == 1  ) then
                
                if ( actual /= " " ) then

                    lexema = lexema // actual

                else    

                    action = lexema

                    lexema = ""

                    estado = 2
                    
                end if
            else if(estado == 2) then

                if (actual /= ";") then

                    lexema = lexema // actual
                    
                else    

                    name = lexema

                    estado = 3

                    lexema = ""

                end if
            else if(estado == 3) then

                if (actual /= ";") then

                    lexema = lexema // actual
                    
                else
                    read(lexema, *) amount
                    estado = 4
                    lexema = ""

                end if

            else if(estado == 4) then

                if (actual /= "#") then

                    lexema = lexema // actual
                    
                else
                    location = lexema
                    exit
                end if
            
            end if
                
        end do
    
    end subroutine analizarCadenaInstrucciones

end module class_Analyzer