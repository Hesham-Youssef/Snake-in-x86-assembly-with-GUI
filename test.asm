DATA SECTION
    number_buffer DB 20 DUP 0
    number_len DB 0

CODE SECTION
    START:
        push 0x00020000
        push 0
        push "sound.wav"
        call PlaySound
        ret
