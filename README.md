# EasyAsm

EasyAsm is an on-device assembly language programming tool for the MEGA65 personal computer. It uses the MEGA65's operating system as part of a complete assembly language development workflow, and maintains a minimal memory footprint when not in use.

Features:
* All 45GS02 CPU instruction types and addressing modes.
* Supports a subset of the [Acme cross-assembler](https://sourceforge.net/projects/acme-crossass/) syntax and features.
* Maintains a minimal memory footprint when running your program or during editing, so you can use the full power of your computer.
* Uses the MEGA65 screen editor's Edit mode for editing assembly language source code.
* Maintains your display settings: screen mode, upper/lowercase, colors.
* Assembles to memory for testing, or to disk files for distribution.
* Can produce a bootstrap loader as part of your program.
* Preserves source code in memory while running your program, and exits cleanly from your program back to the screen editor with source code restored. Can restore source code after an abnormal exit.

EasyAsm is released under the GNU Public License v3. See [LICENSE](LICENSE).

----

**I hope you enjoy EasyAsm! Please consider sponsoring my MEGA65 work: [ko-fi.com/dddaaannn](https://ko-fi.com/dddaaannn)**

----

## Project status

This is EasyAsm version 0.2. It requires **a recent ROM beta version, 920401 or newer,** to assemble to disk.

**All 0.x versions are public beta test releases. Syntax and features may change before the 1.0 release.** Please [file issues](https://github.com/dansanderson/easyasm65/issues) to report bugs, request features, or provide feedback. Thank you for trying EasyAsm!

In v0.2, `!binary` and `!source` are not yet implemented. See also the "Roadmap," below.

## An important note

**Save your work to disk, early and often.**

Writing a program for a microcomputer using the microcomputer itself comes with the inherent risk that a bug in your program will interfere with your programming environment. EasyAsm preserves your source code in memory while you are testing your program, but this cannot be guaranteed to work if the program does something unexpected.

By design, EasyAsm does *not* force you to save your work to disk before testing your program. Please remember to do this yourself.

## Quick reference

* `MOUNT "EASYASM.D81" : BOOT` : install EasyAsm; erases program memory

* Press **Help** to launch the EasyAsm interactive menu

* `EDIT ON` / `EDIT OFF` : enable/disable Edit mode; prompt is `OK.` when enabled
* `DSAVE "..."` : save source file to disk; do this with Edit mode *enabled!*
* `DSAVE "@..."` : save source file to disk, overwriting an existing file
* `MONITOR` : start the Monitor from the `READY.`/`OK.` prompt

## Using EasyAsm

To activate EasyAsm, `BOOT` the `EASYASM.D81` disk:

```basic
MOUNT "EASYASM.D81"
BOOT
```

EasyAsm installs itself in the MEGA65's upper memory, clears program memory, and configures the **Help** key. The prompt changes from `READY.` to `OK.` to indicate that you are in Edit mode. This mode lets you edit a file of text data as if it were a BASIC program, including line numbers. All BASIC commands are still available for direct entry, but now when you type a line with a line number, the MEGA65 understands it as text instead of a line of a BASIC program.

Enter a simple assembly language program:

```
10 !TO "SIMPLE", RUNNABLE
20
30   INC $D020
40   RTS
```

> **Tip:** To insert a blank line in Edit mode (such as line 20 in this example), type the line number, press **Shift+Space**, then press **Return**.

Save the source file to disk:

```basic
DSAVE "SIMPLE.S"
```

> **Tip:** Use `.S` at the end of the source filename to indicate that it is assembly language source. In this case, I saved the source file as `SIMPLE.S`, so I can use the name `SIMPLE` for my program name.

Assemble and run the program:

1. Press **Help**.
2. Select option **1**.

EasyAsm assembles the program, finds no errors, installs it into memory, then runs the program. The program changes the border color, then exits. EasyAsm notices the program has exited using `rts`, and restores the source code into the editor's memory. View the listing:

```basic
LIST
```

Next, assemble the program to disk:

1. Press **Help**.
2. Select option **2**.

EasyAsm assembles the program again, finds no errors, then creates a new PRG file on disk. The `!TO` directive in the program tells EasyAsm that the PRG's filename is `SIMPLE`, and that EasyAsm should make the program "runnable."

Exit out of Edit mode, then load and run the runnable program from disk:

```basic
EDIT OFF

RUN "SIMPLE"
```

> **Note:** Source code and programs share the same memory. Be sure to save your source code file before loading a program from disk.

To load your source file back into memory, return to Edit mode, then `DLOAD` the file `SIMPLE.S`:

```basic
EDIT ON

DLOAD "SIMPLE.S"
```

Assuming your program hasn't erased EasyAsm from memory, it is still present, and you can continue working. See below for a description of EasyAsm's memory requirements.

## How to stop a running program

Because source code and programs often share the same memory, EasyAsm copies your source file to another memory location before it assembles and runs the program. When you assemble and run your program, EasyAsm watches for the program to exit using the `rts` instruction, then copies the source file back into program memory so you can continue to edit it.

That's nice, but it's not always possible—or even desired. A typical machine code program never exits. A broken machine code program might get stuck before it can exit.

Try entering a slightly different program:

```
10 !TO "FOREVER", RUNNABLE
20
30 LOOP:
40   INC $D020
50   JMP LOOP
```

Assemble and run the program. This program changes the border color repeatedly, in an infinite loop. It does not exit.

To interrupt this program and return to BASIC, hold **Run/Stop** and press **Restore**. The program stops, and the MEGA65 Monitor starts. Type `X` then press **Return** to exit the Monitor and return to the `OK` prompt.

Now type `LIST`:

```basic
LIST
```

Uh oh, there's something wrong with the first line! EasyAsm did not see the program exit with the `RTS` instruction, so it didn't get a chance to restore the source file.

When this happens—and it will happen often—use menu option #9 to tell EasyAsm to bring back the source file:

1. Press **Help**.
2. Select option **9**.

Type `LIST` again to see the restored source.

### An edge case about restoring source code

EasyAsm stashes a copy of program memory every time it is invoked, so that it can copy itself into program memory to perform its functions. If your program exits with `RTS`, or EasyAsm exits some other way, it restores the stashed program memory containing your source code on its way out, so you can continue working.

If your program does not exit with `RTS` (as above), the next time you invoke EasyAsm, it avoids stashing program memory, because program memory does not contain your source code: it contains your assembled program. It keeps track of whether it ought to stash source by remembering whether it restored source the last time it was invoked, or if it is being invoked for the first time since you booted the EasyAsm disk.

This is clever in most cases, but it has a small flaw. If you assemble and test your program, interrupt the program such that it doesn't exit via `RTS`, then load new source code into program memory manually (using the `DLOAD` command, or typing `NEW` then entering a new program), the next time EasyAsm is invoked, it will get confused into thinking program memory contains the previously interrupted program and ought to be overwritten by the previously stashed source code.

I have an idea on how to improve this in a later version. For now, just remember to invoke EasyAsm to restore your source code after interrupting a test run of your program.

## Breaking to the Monitor

EasyAsm is designed so that you can use all of the tools available in the MEGA65 operating system as part of your development workflow. This includes the Monitor, a powerful tool for inspecting and experimenting with the state of a machine code program.

Type the `MONITOR` command to start the Monitor at the `OK.` prompt. (Naturally, this also works at the `READY.` prompt when not in Edit mode.)

```basic
MONITOR
```

The Monitor accepts commands through the screen editor. A typical command is a single letter followed by some arguments. You can use the Monitor to inspect CPU registers (`R`) and memory locations (`M1600`), examine code (`D2014`), and call subroutines (`J2032`). Addresses and values are specified in hexadecimal.

To exit the Monitor, enter the `X` command. This returns you to the `OK.` (or `READY.`) prompt.

The Monitor can be especially useful for debugging an assembly language program thanks to a special feature of the MEGA65 operating system. When a program executes a `BRK` instruction, the operating system halts the program, and starts the Monitor. The Monitor displays the status of the CPU registers as they were at the exact point in the program where the `BRK` occurred. You can continue to enter Monitor commands to inspect the state of the system.

Type in then assemble and test this example:

```
10 !TO "BRKDANCE", RUNNABLE
20
30   INC $D020
40   BRK
50   DEC $D020
60   RTS
```

The border changes color, then the program breaks to the Monitor. In the Monitor, enter the `G` command without an argument to continue the program where it left off. The border returns to its previous color, and the program exits. EasyAsm restores the source to program memory.

The Monitor is powerful, but sometimes tricky to use. For example, it is not always possible to resume a paused program, and you may have to exit the Monitor with the `X` command. In this case, you must ask EasyAsm to restore the program source file, as we did after interrupting the program with Run/Stop-Restore.

## Edit mode tips and tricks

Edit mode uses numbered line editing just like BASIC:
* To add a line, type a line number not already in use, followed by the line's text.
* To delete a line, type the line number, then press Return.
* To insert a line between two other lines, choose a line number between those line numbers.
* To display all lines in order, type `LIST`. To display ranges of lines: `LIST 100-200` To display lines, pausing for each page: `LIST P`
* Use the F9 and F11 keys to display the listing as a scrolling display.
* Use the built-in tools like `AUTO` and `RENUMBER` to manage line numbers.

Unlike a BASIC program, Edit mode does *not* preserve line numbers when saving files to disk. When you load your source file again later, it may have different line numbers assigned automatically. To insert more possible line numbers at a given line, use the `RENUMBER` command. For example, to renumber all lines starting at line 450 onward to instead start at 1000, counting by 10: `RENUMBER 1000,10,450-`

Press **Mega+Shift** to toggle lowercase text mode, if you prefer lowercase for assembly language source files. In EasyAsm, labels and strings are case sensitive. Instructions, directives, register names, and hexadecimal literals are not case sensitive.

Edit mode supports entering PETSCII codes into strings similarly to BASIC. When you enter a double-quote (`"`), the editor goes into "quote mode," and PETSCII control codes can be typed as special characters. This works well for string literals in EasyAsm assembly language programs.

To enter a blank line, type the line number, press Shift+Space, then press Return. (This is not normally possible in BASIC.)

If you accidentally type some assembly language code while in BASIC mode (the "READY" prompt), the code will not be entered into memory correctly. To fix this, list the affected lines to the screen while in BASIC mode, type `EDIT ON` to enable Edit mode, then move the cursor up to the lines and press Return on each line. They will be re-entered correctly as assembly source.

The `EDIT ON` command cannot be run from a BASIC program. You can bind it to a function key macro with the `KEY` command to make it easier. (This is how EasyAsm binds to the **Help** key, using `KEY 15`.)

A few ways the MEGA65 behaves differently in BASIC mode vs. Edit mode:

| | BASIC mode | Edit mode |
|-|-|-|
| Prompt | `READY.` | `OK.` |
| A line contains... | BASIC commands | Text |
| File type for DLOAD and DSAVE | PRG | SEQ |
| Saves line numbers to disk | Yes | No |
| Allows blank lines (with Shift+Space) | No | Yes |
| To display a file without loading it: | `LIST "FNAME"` | `TYPE "FNAME"` |


## Invoking EasyAsm

### ... with the Help key

When you install EasyAsm, it configures the **Help** key to open the EasyAsm launch menu. In most cases, you can use this to invoke EasyAsm.

EasyAsm uses the function key macro feature of the MEGA65 to configure the **Help** key, using `KEY 15` in the `AUTOBOOT.C65` file on the EasyAsm disk. If you would rather use the Help key for something else, such as to use your own macro, you can edit `AUTOBOOT.C65` to disable this, or assign it to a different function key.

### ... with a SYS command

As an alternative to the **Help** macro, you can use the following command to open the EasyAsm interactive menu:

```basic
SYS $1E00
```

To invoke one of the menu options non-interactively without opening the menu, use this command, where `A` is the menu option number:

```basic
SYS $1E04,A,0
```

(The `,0` is there to reserve space for an optional argument to pass to EasyAsm. If an argument is introduced in a future version, a script or tool that doesn't set this argument to 0 may accidentally set a random argument value.)

A third-party tool that wishes to integrate with EasyAsm can perform the equivalent load and jump instructions to these addresses. Keep in mind that EasyAsm resets the memory map and base page to the KERNAL's setting when exiting.


## How EasyAsm uses memory

EasyAsm tries to maintain a minimal memory footprint while you are editing your source code, and while your program is running. This allows you to use all the tools at your disposal for editing, and allows your program to use most of the computer, while still retaining a useful on-device workflow.

Of course, EasyAsm has to live somewhere. This is what EasyAsm needs:

* EasyAsm reserves the memory ranges **$1E00-$1EFF** (256 bytes of bank 0) and **$8700000-$87FFFFF** (1 megabyte of Attic RAM). If your program overwrites any of this memory, you will need to reload EasyAsm, and any stashed source code data may not be recoverable.
* EasyAsm reserves the right to overwrite **$50000-$5FFFF** (all 64KB of bank 5) when you invoke EasyAsm. Your program can use this memory while it is running, but the state of this memory may change when EasyAsm is running.

EasyAsm will refuse to assemble to addresses $1E00-$1EFF when assembling to memory, or when assembling a "runnable" program to disk (because the bootstrap routine may use it). This restriction does not apply when assembling to disk in "cbm" mode.

EasyAsm uses program memory ($2001-$F6FF) in three ways:

1. When you `BOOT` the EasyAsm disk at the start of a session, it overwrites program memory with its installer code. After it is installed, it clears program memory.
2. While you are editing source code in Edit mode, your source code occupies program memory. EasyAsm expects to find it there when you invoke the assembler.
3. To test your program, EasyAsm stashes the source code into Attic RAM, copies itself (the assembler) to program memory, assembles the source code to machine code, then installs your machine code in program memory. It runs from this location, as it would when a user loads and runs your program. If the program exits with `rts`, EasyAsm copies the source code back into program memory (overwriting the assembled program) and restores the editing environment.

> **Note:** EasyAsm keeps its own code in Attic RAM while not in use. Attic RAM is not included in MEGA65 Freezer states. It is safe to use the Freezer during an EasyAsm session, but if you start a new session restored from a freeze state, you must run the EasyAsm installer again.


## Assembly language syntax

Wherever possible, EasyAsm uses syntax compatible with the [Acme cross-assembler](https://sourceforge.net/projects/acme-crossass/), so you can enter example code from books and articles for that assembler without changes. Only a subset of Acme features are supported. Source code features exclusive to EasyAsm extend the Acme syntax.

### Comments

A semicolon starts a line comment. EasyAsm ignores everything from the semicolon to the end of the line.

```asm
; This is a comment.

loop:  ; beginning of the loop
  inc $d020  ; increment the background color
```

Naturally, a semicolon that appears inside a character literal or string is not considered a comment.

EasyAsm also supports C-style `//` line comments, similar to Acme.

### Instructions

Instruction names (opcode mnemonics) are canonical for the 45GS02.

```
adc   bbs1  bvs   eor   lbvs  pla   rtn   sty
adcq  bbs2  clc   eorq  lda   plp   rts   stz
and   bbs3  cld   inc   ldq   plx   sbc   tab
andq  bbs4  cle   inq   ldx   ply   sbcq  tax
asl   bbs5  cli   inw   ldy   plz   sec   tay
aslq  bbs6  clv   inx   ldz   rmb0  sed   taz
asr   bbs7  cmp   iny   lsr   rmb1  see   tba
asrq  bcc   cmpq  inz   lsrq  rmb2  sei   trb
asw   bcs   cpq   jmp   map   rmb3  smb0  tsb
aug   beq   cpx   jsr   neg   rmb4  smb1  tsx
bbr0  bit   cpy   lbcc  nop   rmb5  smb2  tsy
bbr1  bitq  cpz   lbcs  ora   rmb6  smb3  txa
bbr2  bmi   dec   lbeq  orq   rmb7  smb4  txs
bbr3  bne   deq   lbmi  pha   rol   smb5  tya
bbr4  bpl   dew   lbne  php   rolq  smb6  tys
bbr5  bra   dex   lbpl  phw   ror   smb7  tza
bbr6  brk   dey   lbra  phx   rorq  sta
bbr7  bsr   dez   lbsr  phy   row   stq
bbs0  bvc   eom   lbvc  phz   rti   stx
```

EasyAsm supports explicit 16-bit branch instructions, using Acme syntax: `lbcc`, `lbcs`, `lbeq`, `lbmi`, `lbne`, `lbpl`, `lbra`, `lbsr`, `lbvc`, `lbvs` Neither EasyAsm nor Acme support automatic promotion of 8-bit branch instructions to 16-bit. The assembler will report an error if an 8-bit branch is too short.

EasyAsm supports both `cpq` (Acme) and `cmpq` (Monitor, MEGA65 manual) as spellings of that instruction. It also supports both `rtn #...` (Acme) and `rts #...` as synonyms, just for that addressing mode.

Instructions that operate on the accumulator or quad register as an alternative to a memory location are sometimes spelled with an `A` or `Q` in the place of an argument. Omit these for EasyAsm, as you would for Acme or the Monitor. For example, to logical-shift right the accumulator, use `lsr`, not `lsr a`.

Acme [erroneously](https://sourceforge.net/p/acme-crossass/tickets/22/) accepts `ldq (zp)` and `ldq [zp]`, but not `ldq (zp),z` and `ldq [zp],z`. The latter are actually more correct, because these instructions do honor the Z index, even though they also overwrite the Z register. For compatibility with Acme, EasyAsm supports both syntaxes. I recommend using the `ldq (zp),z` syntax for readability.

The following are examples of syntax for the addressing modes. See the [MEGA65 Compendium](https://files.mega65.org/?id=d668168c-1fef-4560-a530-77e9e237536d) for a complete description of which instructions support each addressing mode.

* `lda #$07` : immediate
* `lda $fe` : zero page direct
* `lda $1600` : address direct
* `lda $fe,x` : zero page direct, X-indexed
* `lda $1600,x` : address direct, X-indexed
* `lda ($fe,x)` : zero page indirect X-indexed
* `lda ($fe),y` : zero page indirect, Y-indexed
* `lda ($05,sp),y` : stack indirect, Y-indexed
* `lda [$fe],z` : 32-bit zero page indirect, Z-indexed
* `lsr` : "accumulator addressing" (implied "A" argument)
* `lsrq` : "quad register addressing" (implied "Q" argument)
* `jmp ($fffe)` : indirect jump

Multiple instructions can appear on a single line, separated by colons. `lda #0 : sta $d020`

### Values

EasyAsm uses 32-bit signed integer values in expressions for arguments. When a value is assembled into an instruction or used by an assembler directive, it must be in the range expected by how it is used:

* Address argument
  * 16-bit absolute addresses must be a value in the range 0 ($0000) to 65,535 ($FFFF).
  * 16-bit addresses are assembled to two bytes in little-endian order: $FFFC assembles to $FC $FF.
  * 8-bit base page (ZP) addresses must be a value in the range 0 ($00) to 255 ($FF).
* Immediate mode argument
  * Expected to be either a signed value in the range -128 to 127, or an unsigned value in the range 0 to 255.
  * It is assembled to one byte, using two's complement for negative values: 255 and -1 both assemble to $FF.
  * As the only exception, the `phw` instruction accepts a 16-bit value in Immediate mode.
* Assembler directives that take numeric arguments have their own range requirements.
  * The `!32` directive will render a complete 32-bit value as two's complement little-endian: $FFFFFFFC and -4 both assemble to $FC $FF $FF $FF.

Number literals:

* Decimal: `12345`, `-27`
* Hexadecimal: `$FFFC`
  * Letter digits are case insensitive: `$fffc` and `$FFFC` are equivalent.
* Binary: `%0010110`
  * Binary literals can also use `.` for `0` and `#` for `1`, for more easily representing bitmap graphics: `%..#.##.`
* PETSCII character: `'p'`
  * When used with the `!scr` directive, this is translated into a screen code if applicable. Otherwise it is interpreted as the PETSCII code of the character that appears in the source file.
  * EasyAsm does not support backslash-escape sequences. To represent the PETSCII code for the single-quote character `'`, put it in single quotes: `'''`. (As an alternative, the PETSCII code for a single-quote character is 39 ($27).)

To specify a negative number literal, use negation syntax: `-27` If a literal specifies all 32 bits and bit 31 is high, this will be treated as a two's complement negative number: `$FFFFFFFC` Negating such a literal will negate the number: `-$FFFFFFFC` and `4` are equivalent.

Some assembler directives accept text strings as arguments, surrounded by double-quotes: `"string"` These are not values and cannot be used in expressions. The `!pet` and `!scr` directives render strings into sequences of character bytes (PETSCII or screen codes, respectively).

### Entering PETSCII control codes

Similar to editing a BASIC program, you can enter PETSCII control codes inside double-quoted strings by typing the key that would normally perform that code, such as cursor movement, color changes, or clearing the screen.

When you type a double-quote character (`"`), the editor switches to *quote mode,* and keys that would type PETSCII control codes instead enter inverse symbols that represent those codes in the string. When you type the closing double-quote, the editor switches off quote mode.

Double-quoted string arguments to `!pet` can contain PETSCII control codes entered in this way. In the following example, type `{CLR}` as Shift + Clr, and `{WHT}` as Ctrl + 2.

```asm
  jsr primm
  !pet "{CLR}{WHT}Hello world!",0
```

Quote mode is also active temporarily for spaces inserted with the Shift + Inst key. This is convenient for inserting characters within quoted strings without having to type double-quotes just to activate quote mode.

When you enter a line by pressing the Return key, only PETSCII codes inside of a pair of double-quotes are interpreted as PETSCII codes. (This is true when editing BASIC programs as well as in Edit mode.) A PETSCII code outside of double-quotes is interpreted by the editor as a symbol character.

For EasyAsm, this means that it is not practical to enter PETSCII codes as single-quoted character literals. Instead, use number literals or named constants to refer to PETSCII control codes.

```asm
chr_clr = 147
chr_esc = 27

  lda #chr_clr
  jsr bsout

  lda #chr_esc
  jsr bsout
  lda #'5'
  jsr bsout
```

### Labels

A label is a name associated with a value. Unlike a "variable" in some programming languages, a label's value does not change: the assembler determines its value by examining the source code, then uses that value wherever the label appears.

A label can be assigned a value using the `=` sign, like so:

```asm
bgcolor = $d020

  inc bgcolor
```

A label can also be assigned the address of an instruction in the program:

```asm
loop:
  inc bgcolor
  jmp loop
```

The colon `:` is optional.

A global label name must start with a letter, either lowercase or uppercase. Subsequent characters can be either a letter (lowercase or uppercase), a number, back-arrow (ASCII underscore), a dot (`.`), or Mega + `@` (an underscore-like character in PETSCII).

A label cannot be similar to an instruction mnemonic. It can be similar to other keywords. For example, `xor` can be used as a label (case sensitive), even though it is also an operator (case insensitive). `lda` cannot be used as a label.

> **Tip:** If you choose to use uppercase + graphics text mode for assembly programming, I recommend limiting the use of shifted letters in label names. They're allowed because they are uppercase letters in lowercase text mode, but they are difficult to remember how to type, and some are difficult to distinguish. For example, Shift + G and Shift + T both appear as vertical lines in uppercase text mode.

### "Cheap" local labels

EasyAsm supports global labels and Acme-style "cheap" local labels. A global label must be unique across all labels in the source file. A "cheap" local label starts with an `@` sign, and is only valid up to the next global label. This allows subroutines with global names to have local labels with useful names that can be reused by other subroutines.

```asm
flash_border:
  lda #15
@loop:
  sta $d020
  dec
  bne @loop
  rts

flash_background:
  lda #15
@loop:
  sta $d021
  dec
  bne @loop
  rts
```

A "cheap" local label name starts with `@` followed by a letter. Subsequent characters follow the rules for labels.

### Relative labels

EasyAsm supports relative labels for code. A relative label is a sequence of one or more minus `-` signs, or one or more plus `+` signs: `-` `--` `---` `+` `++` `+++` When an instruction uses a relative label as an address argument, it refers to the closest code label with that name in the source code. A relative label of minus signs scans upward to the closest assignment, and a relative label of plus signs scans downward.

```asm
flash_border:
  lda $d020
  cmp #3
  beq +
  lda #15
- sta $d020
  dec
  bne -
+ rts

flash_background:
  lda #15
- sta $d021
  dec
  bne -
  rts
```

### Expressions

An argument's value or a label's value can be calculated using a mathematical expression. An expression can be a number literal, a label, or one or two expressions combined with a mathematical operator. EasyAsm calculates the value of the expression when assembling the program.

EasyAsm supports the following operators, listed in precedence order:

| Syntax | Definition | Notes |
|-|-|-|
| `!v` | Bitwise complement | |
| `v ^ w` | To the power of | Up-arrow |
| `-v` | Negate | |
| `v * w` | Multiply | |
| `v DIV w` | Integer divide | |
| `v % w` | Remainder | |
| `v + w` | Add | |
| `v - w` | Subtract | |
| `v << w` | Shift left | |
| `v >> w` | Arithmetic shift right | |
| `v >>> w` | Logical shift right | |
| `<v` | Low byte of | |
| `>v` | High byte of | |
| `^v` | Bank byte of | Up-arrow |
| `^^v` | Megabyte byte of | Up-arrow |
| `v & w` | Bitwise And | |
| `v XOR w` | Bitwise Exclusive Or | |
| `v \| w` | Bitwise Or | Mega + period |

EasyAsm does not support fractional number values, and so does not have a fractional division operator (`/`).

EasyAsm does not support Boolean values, and so does not have conditional operators. (This would primarily be used with conditional assembly, which EasyAsm also does not support.)

The "megabyte" operator `^^` is exclusive to EasyAsm, as a companion to low (`<`), high (`>`), and bank (`^`) byte selectors. The megabyte operator selects the highest byte of a 32-bit value.

The power operator is right-associative: `x^y^z` = `x^(y^z)` In EasyAsm, it is an error for an exponent to be negative.

To type the power operator, type the up-arrow character (next to the Restore key). To type the bitwise-or operator, type Mega + period.

> **Tip:** Enter the command `FONT A` and switch to lowercase mode (Mega + Shift) to display certain characters as their ASCII equivalents. To type the ASCII-specific characters:
> * Back-arrow: underscore (`_`)
> * Up-arrow: caret (`^`)
> * Mega + back-arrow: backtick
> * Mega + period: vertical bar (`|`)
> * Mega + comma: tilde (`~`)
> * Mega + forward-slash (or £): backslash (`\`)
>
> Remember that you must be in lowercase mode with this font setting to see the ASCII characters.

### Parentheses and brackets

Both parentheses `(` `)` and square brackets `[` `]` can be used to group terms in expressions to influence the order of operations. The two bracket types can be used interchangeably, but must be used in matching pairs.

The indirect addressing modes also use brackets in their syntax. In this case, the type of bracket selects the addressing mode (as above). Addressing mode brackets are unambiguous from mathematical expression brackets by the following rule:

*If the entire address argument expression is surrounded by brackets, the outermost brackets are part of the addressing mode.*

Examples:

```asm
  lda ($fe),y          ; indirect ZP, Y-indexed
  lda ($ff-1),y        ; indirect ZP, Y-indexed
  lda (($ff-1)-foo),y  ; indirect ZP, Y-indexed
  lda ([$ff-1]-foo),y  ; indirect ZP, Y-indexed
  lda ($ff-1)-foo,x    ; direct ZP, X-indexed
  lda [$ff-1],y        ; 32-bit indirect ZP, Y-indexed
  lda ([$ff-1]),y      ; indirect ZP, Y-indexed
```

### Zero-page address arguments

The absolute addressing mode and the base page addressing mode have the same assembler syntax, despite being separate instructions with separate behaviors. For example:

```asm
  lda $fe            ; load index $fe off the base page
  lda $16fe          ; load absolute address $16fe
```

If the address expression evaluates to a value larger than 255, then EasyAsm uses absolute addressing.

If the address expression evaluates to a value between 0 and 255, EasyAsm disambiguates using the following procedure:

1. If the address expression is a value literal *or* a symbol defined using a value literal, and the value literal has one or more leading zeroes, EasyAsm uses absolute addressing: `$00fe` Otherwise it uses base page addressing: `$fe`
2. If the value can be calculated in the first pass of the assembler, i.e. all symbols in the expression are defined earlier in the source text, EasyAsm uses base page addressing: `$fc + earlierlabel`
3. Otherwise EasyAsm assumes absolute addressing, to be calculated in a subsequent pass, even if the result is less than 256: `$fc + laterlabel`

To clarify potential confusion in your source code, you can suffix the instruction mnemonic with `+1` to force base page addressing, or `+2` to for absolute addressing. If base page addressing is requested and the value is larger than 255, EasyAsm reports the error.

```asm
  lda+1 $00fe        ; force base page addressing: $fe
  lda+2 $fe          ; force absolute addressing: $00fe

  varname = $fe
  lda+2 varname      ; force absolute addressing: $00fe
```


## The program counter

EasyAsm maintains a *program counter* (PC), the address where the next assembled instruction will appear in memory. As the assembler encounters each CPU instruction, it generates the machine code for the instruction, places it at the address in the program counter, then increments the program counter by the size of the instruction.

A program can refer to the current value of the PC in code using an asterisk `*`. A program can use the value in an expression. It can also assign a new value to the PC, similar to a label, to influence the assembly process.

```asm
* = $c000    ; Set the PC to $C000

change_background:   ; change_background = $c000
  inc $d020   ; Assembles to three bytes
  rts         ; Assembles to one byte

blah = *     ; Label "blah" is now set to $c004

blah:        ; (This does the same thing.)
  ; ...
```

A typical program does not have to change the PC. It can use the `!to "...", runnable` directive to start a runnable program to be saved to the given filename. This sets the PC automatically to an address appropriate to the bootstrap code that EasyAsm puts at the beginning of the program data.

### Segments

A set of instructions and data assembled to a contiguous region of memory is known as a *segment*. A typical program consists of one segment.

When program code assigns a new value to the program counter and this is followed by an instruction or data directive, EasyAsm starts a new segment at that address. The assembler keeps track of all the contiguous segments formed by assembled instructions and data.

In EasyAsm, it is an error if two segment overlap, ala Acme's "strict segments" mode. When assembling to memory, it is an error if a segment overlaps EasyAsm's variable memory region, $1E00-$1EFF.

### "Pseudo-PC" not yet supported

EasyAsm currently does not support Acme's "pseudo-PC" feature, which allows for a section of code to be assembled as if the PC were a particular value, but the segment is stored in the PRG file consecutively with the surrounding code. (It would be the program's responsibility to copy the code to the correct location in order to run it.)


## Assembling to disk

When assembling to memory, EasyAsm writes each segment to the requested memory locations.

When assembling to disk, EasyAsm offers several options: writing a contiguous file, writing segments to separate files, or saving a single segment as a runnable program.

> **Tip:** The "runnable" file is the most convenient way to write a program that can be run with the `RUN` or `BOOT` command, which is how most users expect to invoke a program. EasyAsm generates the bootstrap code that you would normally have to include manually in an Acme program listing.

### Writing a contiguous file

The `!to` directive sets a filename and writing mode for assembling subsequent code to disk. Using the `cbm` mode, this creates a PRG file, with the address of the first assembled instruction that follows as the load address. This requires that the source code set the PC before the first instruction, so the `!to` directive knows the starting address.

```asm
!to "routines", cbm
* = $a000

change_background:
  inc $d020
  rts
```

If a single `!to` directive is followed by more than one segment, EasyAsm creates a PRG file that starts at the segment with the lowest address. Each segment is followed by a region of empty data, such that each segment is written into its starting address when the file is loaded. This can be useful if the separator regions are expected to be small, such as to align code to specific addresses.

This example generates one file with the first segment, a region of empty data to align the next segment, followed by the second segment.

```asm
!to "routines", cbm

* = $7400
  jsr change_border
  ; ...
  rts

; (...empty data generated here...)

* = $7f00
change_border:
  inc $d020
  rts
```

If the gap between segments is large, this could result in excess use of disk space and loading time. EasyAsm offers other options to avoid this scenario.

### Writing segments to separate files

EasyAsm source code can request that segments be written to different files by providing the `!to "...", cbm` directive more than once. It is the program's responsibility to load each segment file from disk into the appropriate memory location.

This example generates two files, one for each segment.

```asm
!to "screen", cbm
* = $7400

  ; (Some disk code to load the "routines" file...)

  jsr change_border
  ; ...
  rts

!to "routines", cbm
* = $a000

change_border:
  inc $d020
  rts
```

### Generating a runnable program

In EasyAsm, the `!to "...", runnable` directive creates a program file that starts with a bootstrap routine. A user can load and `RUN` this program from the `READY.` prompt. The program starts with the first instruction after the `!to "...", runnable` directive.

`!to "...", runnable` sets the PC automatically. It is not necessary (and not allowed) to set the PC explicitly in this case.

```asm
!to "myprog", runnable

  inc $d020
  rts
```

A runnable program is only allowed to have one segment, at the default memory location.


## Using disk drives

Assembler directives that refer to files on disk (`!to`, `!source`, `!binary`) always use the current "default disk" unit. BASIC 65 initially sets this to unit 8. You can change the default disk with the `SET DEF` command.

```basic
SET DEF 9
```

BASIC disk commands use this default, and allow overriding the default with the `U` argument. EasyAsm does not currently have a way to override the default selectively. Take care to set the default disk unit when managing files across multiple disks.

```basic
SET DEF 9
EDIT ON
DLOAD "MYPROG.S",U8  : rem: Loads from unit 8
SYS $1E04,2,0        : rem: Assembles to unit 9
```


## Assembler directives (pseudo-operands)

EasyAsm supports the following assembler directives.

### `!to`

```asm
!to "...", <mode>
```

Sets the output file and mode when assembling to disk. `<mode>` can be `cbm` (PRG with address) or `runnable` (bootstrap routine).

A `cbm` file is a simple PRG file. It must be followed by assignment of the program counter before the first instruction, e.g. `* = $1600`. A `cbm` file can contain one or more segments. EasyAsm will render the file with gaps of zeroes between the segments, so they are positioned correctly when the file is loaded at the lowest segment starting address. Segments can be described in any order, but must not overlap in memory.

A `runnable` file is a PRG file that can be loaded with `DLOAD` and invoked with `RUN`. The program counter cannot be changed, and the file must contain only one segment. EasyAsm locates the program at the beginning of BASIC program memory, and prepends a BASIC bootstrap routine.

A source file can provide more than one `!to` directive to create multiple files. Each file will contain the segments defined up to the next `!to` directive, or to the end of the source file otherwise. Note that none of the segments in the program are allowed to overlap, even when being saved to separate files, so that the program can also be assembled to memory.

Assembling to disk will report an error if any instructions or data appear before the first `!to` directive. This is not an error when assembling to memory.

EasyAsm directives that refer to files on disk use the current "default disk" unit. Use the `SET DEF` command to change the default disk.

### `!byte` or `!8`, `!word` or `!16`, `!32`

```asm
!byte <val> [,<val> ...]
!8 <val> [,<val> ...]
!word <val> [,<val> ...]
!16 <val> [,<val> ...]
!32 <val> [,<val> ...]
```

Assembles value expressions to data. `!byte` (or synonym `!8`) accepts 8-bit values. `!word` (or synonym `!16`) accepts 16-bit values. `!32` accepts 32-bit values. Multi-byte values are rendered as little-endian.

### `!fill`

```asm
!fill <amt> [, <val>]
```

Assembles a given number of bytes of data as multiple instances of a single byte value. The default value is $00. If a byte value is provided, that value is used for every byte.

### `!pet`, `!scr`

```asm
!pet "..." [, "..."]
!scr "..." [, "..."]
```

Assembles arguments that can include character strings as a series of bytes. `!pet` renders character strings and character literals as PETSCII codes, as written directly in the PETSCII source file. `!scr` converts PETSCII characters in double-quoted strings and single-quoted character literals to VIC screen codes. Number expressions are byte values and are rendered verbatim in either case.

String data does not automatically add a null terminator. If a null terminator is desired, end the directive with a 0 byte.

```asm
!pet "hello world!", 0
```

EasyAsm does not support backslash-escape character sequences. To include a double-quote character in a `!pet` or `!scr` directive, end the string, then use a byte value of 34 ($22).

```asm
!pet "i said, ", 34, "hello world!", 34, 0
```

If a PETSCII control code appears in a string or character literal passed to `!scr`, it is converted to the screen code for a space ($20).

### `!source`

```asm
!source "..."
```

***v0.1: Not yet implemented.***

Loads a source file from disk to be assembled as code at the given location.

EasyAsm directives that refer to files on disk use the current "default disk" unit. Use the `SET DEF` command to change the default disk.

### `!binary`

```asm
!binary "..." [, <size> [, <skip>]]
```

***v0.1: Not yet implemented.***

Loads a binary file from disk to be assembled as data at the given location. Without arguments, the entire file is included. `<size>` limits the number of bytes to include. `<skip>` starts assembling data that many bytes into the file.

EasyAsm directives that refer to files on disk use the current "default disk" unit. Use the `SET DEF` command to change the default disk.

### `!warn`

```asm
!warn "..." [, "..."]
```

Prints a given PETSCII message during the assembly process. Arguments are string literals or number expressions. A number expression argument is printed as both decimal and hexadecimal, useful for debugging, such as: `123 ($7B)`

No other Acme-style message directives are supported. They're only useful for conditional assembly, which is not supported.


## Acme assembler compatibility

EasyAsm tries to provide a subset of the features of the Acme assembler, using compatible syntax. Not all features of Acme are supported, and a few features are exclusive to EasyAsm.

### Acme features that are not supported

EasyAsm has the following limitations compared to the Acme assembler.

* "Edit mode" in-memory source size limit of 44 KB
* No "plain" file type (without the PRG starting address)
* No macros
* No conditional or looping assembly
* No symbol list output
* No fractional number values or fractional division operator
* No Boolean values or conditional expressions
* No zones (but "cheap" local symbols are supported)
* No CPUs other than the 45GS02 (`!cpu m65`)
* No assembler directives (pseudo-ops) or directive aliases other than those listed
* No mathematical functions or operators other than those listed
* No `0x` and `0b` syntax for hex and binary literals (use `$...` and `%...`)
* No octal literals
* No way to set a "pseudo-PC"

### Features exclusive to EasyAsm

Here is a quick summary of features available in EasyAsm that are not available in Acme:

* PETSCII character encoding of source files
* Single-quote character literal: `'''`
* Double quote characters not allowed in double-quoted strings
* `!to "...", runnable`
* Assemble to multiple files from one source file, with multiple `!to` directives
* The "megabyte" (`^^`) selector operator
* Instruction synonyms: `cmpq` for `cpq`, `rts #...` for `rtn #...`

## Roadmap

Features I intend to add for version 1.0 of EasyAsm:

* `!binary`
* `!source`
* List symbol definitions
* List assembled bytes alongside source code

Some things I hope to add someday, if there's enough interest in this project:

* Output "plain" files (without the PRG starting address)
* Runnable with multiple segments or non-default starting address
* Improved Freezer support (optionally host EasyAsm in chip RAM)
* Pseudo-PC regions
* Zones and real locals
* Macros
* Conditional assembly, conditional expressions
* A nice text editor

## Building EasyAsm

Building EasyAsm from source requires the following tools:

* [Acme assembler](https://sourceforge.net/projects/acme-crossass/)
* [GNU Make](https://www.gnu.org/software/make/)
* [Python 3](https://www.python.org/)
* [The d64 Python package](https://pypi.org/project/d64/)
    * To install: `python3 -m pip install d64`
* `petcat`, [from VICE](https://vice-emu.sourceforge.io/) or [direct download](https://files.mega65.org?id=9561505c-a36d-4d3e-b158-d52a718e818e)
    * This requires a recent version capable of producing MEGA65 programs.

To build `easyasm.d81`:

```
make easyasm.d81
```

This project uses `makedisk.py`, my own tool for producing D81 disk files. The contents of the disk are described in `files.json`. It has built-in support for executing `petcat` to convert `.bas` files to PRG files, and also converts `.txt` files to `TYPE`-compatible SEQ files with nice formatting (lowercase text). See [makedisk.py](makedisk.py) for a complete description.
