-- CPSC 312 - 2018 - 

-- Algorithm for Toy Encryption, Symmetrical Key, Cipher--

module Decrypt where
    
    -- Encrypt a message --

    encrypt key msg 
        | key > 0   = encrypt (key - 1) (map shiftForward msg)
        | key == 0  = msg

    -- Cipher Shifters --
    
    shiftForward char
        | char == ' '   = '.'
        | char == '.'   = '.'
        | char == 'z'   = 'a'
        | char == 'Z'   = 'A'
        | otherwise     = succ char

    shiftBack char
        | char == '.'   = ' '
        | char == ' '   = ' '
        | char == 'a'   = 'z'
        | char == 'A'   = 'Z'
        | otherwise      = pred char

    -- Decrypt a message --

    decrypt key msg
        | key > 0   = decrypt (key - 1) (map shiftBack msg)
        | key == 0  = msg



    

