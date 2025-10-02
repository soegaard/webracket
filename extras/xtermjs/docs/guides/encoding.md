---
title: Encoding
category: Guides
---

### Input

`Terminal.write` supports these input types with the following encoding:
- `string` interpreted as UTF-16
- `Uint8Array` as sequence of UTF-8 byte values

The input decoders are stream aware, thus will compose codepoints from consecutive multibyte chunks.

### Output

- `Terminal.onData`

  contains real string data with any valid Unicode codepoint, thus the payload should be treated as UTF-16/UCS-2. For OS interaction this data should be converted to UTF-8 bytes (automatically done by [node-pty](https://github.com/microsoft/node-pty)). If you need legacy encoding support, see [below](#legacy-encodings).

- `Terminal.onBinary`

  contains raw binary data. For easier consumption the payload is a `string` type, but should be treated as a binary string, where a codepoint directly maps to the 8-bit byte value. The payload should never contain a higher codepoint than 255. If you find a higher codepoint, strip the high bits (e.g. `data[n] & 0xFF`) before processing and file an issue, as it is most likely a problem on the event triggering side. In nodejs you can consume the payload with the `'binary'` encoding, which automatically takes care of the high bits (e.g. `Buffer.from(data, 'binary')`). xterm.js currently uses `onBinary` only for legacy mouse reports, that cannot be encoded in UTF-8.  
  **Caveat:** Watch out for automatic UTF-8 conversions done by some nodejs interfaces with `string` type, always go with the buffer variant if possible.

### Legacy Encodings

`xterm.js` does not support any legacy encoding and probably never will. If you have to deal with older systems or programs that dont understand UTF-8, we strongly suggest to use a streamline transcoder like [luit](https://linux.die.net/man/1/luit) to translate between the foreign encoding and UTF-8. `luit` was designed with terminal data streams in mind and can handle most scenarios with escape sequences correctly. If it does not suit your purpose or is not available for your system refer to general converters that most environments offer (e.g. [iconv](https://en.wikipedia.org/wiki/Iconv)).

__example:__ luit using latin-1 for a single program
```bash
$> luit -encoding ISO8859-1 -- command
```

__example:__ luit transcoding a ssh connection
```bash
$> LC_ALL=fr_FR luit ssh legacy-machine
```

`luit` and `iconv` can also be used on integration level, see [Integration issues / PTY bridge](#integration-issues--pty-bridge).

### Typical encodings issues with xterm.js

#### Wrong locale

On modern systems make sure to set your `locale` to an UTF-8 variant representing your localization needs, e.g. for a German based system it may look like this:

```bash
$> locale
LANG=de_DE.UTF-8
LANGUAGE=de_DE
LC_CTYPE="de_DE.UTF-8"
LC_NUMERIC="de_DE.UTF-8"
LC_TIME="de_DE.UTF-8"
LC_COLLATE="de_DE.UTF-8"
LC_MONETARY="de_DE.UTF-8"
LC_MESSAGES="de_DE.UTF-8"
LC_PAPER="de_DE.UTF-8"
LC_NAME="de_DE.UTF-8"
LC_ADDRESS="de_DE.UTF-8"
LC_TELEPHONE="de_DE.UTF-8"
LC_MEASUREMENT="de_DE.UTF-8"
LC_IDENTIFICATION="de_DE.UTF-8"
LC_ALL=
```

We cannot explain the `locale` system here in detail, just a few notes:
- For program input/output the important entry is `LC_CTYPE`.
- `LC_ALL` can be used to override all other settings.
- Other entries can further customize special localization needs, like collation order or date format.
- Empty entries default to `"C"` (this is treated differently depending on the clib, in summary it means that a character representation is bound to one byte and only ASCII can be used savely)

If you find `LC_CTYPE` and `LC_ALL` being empty or set to some string not containing `'UTF-8'` in the name, programs will not be able to correctly deal with non ASCII characters with xterm.js. To fix this, either:
- overwrite `LC_CTYPE` in local session only, e.g. by `export LC_CTYPE="xy_AB.UTF-8"` (selected locale must be installed)
- change system locale to UTF-8 variant (consult your OS documentation about localization)

If you cannot change the locale to some UTF-8 variant (some old OS might not provide UTF-8 support), see [Legacy Encodings](#legacy-encodings).


#### Wrong $LANG environment variable

Some programs might not rely on locale settings directly, instead evaluate the environment variable `$LANG`. Typically this variable contains the default locale of the system, but can be used to customize this setting for individual programs.

Make sure that `$LANG` contains an UTF-8 capable locale (if in doubt, use the default locale). On systems without UTF-8 support refer to [Legacy Encodings](#legacy-encodings).

#### Integration issues / PTY bridge

xterm.js is heavily tested against `node-pty` as it is shipped mostly with Node.js based solutions. With `node-pty` the integration is straight forward for most cases:

```Javascript
pty.onData(recv => terminal.write(recv));
terminal.onData(send => pty.write(send));
```

Note that `node-pty` assumes UTF-8 as default encoding, thus interprets any bytes from OS-pty as UTF-8. Of course this will not work anymore if the OS-pty does not send or expect UTF-8 data. To solve this on integration level, `luit` can be used by wrapping the initial shell command:

__before:__
```Javascript
const pty = node_pty.spawn(shell, [], {...});
```

__after:__
```Javascript
const pty = node_pty.spawn('luit', ['-encoding', 'ENCODING', '<other options>', '--', shell], {...});
```

With the npm package `iconv` a similar result can be achieved on individual chunk level:

```Javascript
const Iconv  = require('iconv').Iconv;
const receiveConverter = new Iconv('ISO-8859-1', 'UTF-8');
const sendConverter = new Iconv('UTF-8', 'ISO-8859-1');
...
const pty = node_pty.spawn(shell, {encoding=null, ...});
pty.onData(recv => terminal.write(receiveConverter.convert(recv)));
terminal.onData(send => pty.write(sendConverter.convert(send)));
```

Both solutions have several drawbacks that you will have to work around if you want legacy encoding support on integration level (e.g. for a runtime encoding switch offered to users):
- `luit`:
  - No Windows support.
  - Sits interactively on the pty stream, thus encoding cannot be switched easily without destroying/recreating the pty. To circumvent this, `luit` supports a `-c` cmdline switch where it can act as an independent subprocess. Your success with this might vary.
- `iconv`:
  - No stream support. For multibyte encodings (e.g. CJK) you have to implement countermeasures to catch partly transmitted byte sequences before transcoding the data.
  - `luit` supports rewriting of several terminal sequences (7bit rewrites, changing character sets) while `iconv` does not know anything about terminal sequences at all. In general this should not be a problem in conjunction with xterm.js, it supports 8-bit sequences just fine and will fall back to UTF-8 for unknown character set commands.

For integrations not based on `node-pty` please refer to [Input](#input) and [Output](#output). A rule of thumb here is - always treat xterm.js side as UTF-8 and transcode non-UTF-8 OS-pty data accordingly in both directions. Since most languages and transports have proper UTF-8 support these days it might be a good idea to transcode the data as close as possible to the raw OS-pty byte sink. If in doubt check encoding support of every single component the data has to go through.

## Rationale & Background

__Do I really have to care for text encodings, isn't a JS string supposed to work out of the box everywhere?__

Short answer: It depends.

Long answer:

For most programmers, text encoding issues are a nightmare to deal with. Many programming languages / environments try to
hide that complexity behind default assumptions for their string types which makes dealing with string data
straight forward. For Javascript the web consortium has done a great job to standardize this in an almost uniform way (HTML5 as UTF-8, Javascript strings as UCS-2/UTF-16), which can be transcoded transparently. Thus most web / JS programmers dont face encoding issues anymore, if they stay within these assumptions.

But as soon the data leaves that well defined environment the situation gets worse - the interfaces involved have to agree about the correct data representation and their meaning. Thats a common problem with adapters to databases or other datasinks.

Xterm.js in particular has to digest data from and write to the operating system in the correct format. At that point data might have to be re-encoded, if the OS and xterm.js dont follow the same text encoding. Although we see a standardization towards Unicode (UTF-8) as exchange format in all major OS, older systems or certain programs still might use legacy encodings.


__Refresher - what is a text encoding?__

An encoding is a mapping of a piece of information to some meaning, a convention what it represents / stands for. 
In a computer system a text encoding typically maps certain bit sequences (numbers) to characters (charmap).

Historically text encodings evolved with the invention of automated information processing and the need to represent natural language systems in a machine / memory state (from Morse code, Baudot, ASCII up to Unicode variants). The earlier encodings were limited to certain languages (like ASCII with english letters only) and had a smaller memory footprint (ASCII is only 7 bit per character). With the broader adoption of computer systems worldwide in the 70s and 80s this was a big limitation and led to the standardization of language specific 8-bit encodings (DOS encodings, ISO 8859), which can encode most latin based languages and several others with small alphabets. Languages with really big alphabets like CJK still could not be represented, thus several multibyte encoding systems were developed. This led to a nightmare of different encoding systems and eventually to the idea to standardize this further in "one to rule them all" - Unicode (first release 1991).

See the [Wikipedia entry](https://en.wikipedia.org/wiki/Character_encoding) for more background on the evolution of text encodings.


__What encodings are currently in use?__

That is somewhat tricky to answer. In general we see a replacement of all legacy encodings by Unicode variants on all systems. But its still possible get in contact with legacy encodings on older systems / machines. Therefore we gonna list here the most common:

- [ASCII](https://en.wikipedia.org/wiki/ASCII)  
  fundamental 7-bit encoding, unlikely to go anytime soon as it is weaved into ISO-8859 encodings and Unicode (1:1 compatible). 

- [ISO-8859 family](https://en.wikipedia.org/wiki/ISO/IEC_8859)  
  several 8-bit encodings. 8859-1 (western/latin-1) was the standard of HTML4 and in many older POSIX systems. This family has several derived encodings like [Windows-1252](https://en.wikipedia.org/wiki/Windows-1252). Still in use, but mostly replaced by Unicode encodings.

- other 8-bit encodings  
  Beside the ISO-8859 family there were other 8-bit encodings quite common in the 80s. Most common was [CP-437](https://en.wikipedia.org/wiki/Code_page_437) as default on IBM-PCs (DOS encoding). CP-437 and its other language variants (like CP-850) can still be found on some systems (e.g. as legacy encoding in cmd.exe on Windows). Not used in the web context.

- [CJK encodings](https://en.wikipedia.org/wiki/CJK_characters)  
  This is the most heterogeneous group of encodings with very different encoding paradigms. Most can be seen as ancient and are replaced by Unicode. If interested, see a list of formerly common CJK encodings [here](https://en.wikipedia.org/wiki/CJK_characters#Encoding).

- [Unicode](https://en.wikipedia.org/wiki/Unicode)  
  Standard for most scenarios these days replacing 8-bit and CJK encodings. The Unicode specification maintains a really big table of 1,114,112 codepoints in several 16-bit planes (U+0000 to U+10FFFF). Most of these codepoints are not defined yet, they are subject to ongoing specifications done by the Unicode Consortium. Unicode is backwards compatible to ASCII (incorporates ASCII as a subset).  
  Note that the Unicode table is not an encoding itself, it is treated as an abstract character room, that an Unicode compatible encoding should be capable to represent. Thus the real memory representation is left to a certain Unicode encoding. Thats where the [UTF encoding family](https://en.wikipedia.org/wiki/Unicode#UTF) enters the stage (beside some others not listed here).

  Due to being really big a direct 1:1 mapping of codepoints is not feasible anymore in many circumstances as every codepoint (character position in the Unicode table) would take at least 2^21 bits (or 4 bytes memory aligned). Thus several UTF encodings were developed to optimize the handling in regards of memory consumption and / or string processing speed:
    - [UTF-8](https://en.wikipedia.org/wiki/UTF-8)  
    Developed by the father of UNIX [Mr. Ken Thompson](https://en.wikipedia.org/wiki/Ken_Thompson), UTF-8 is optimized to reduce memory consumption and bandwidth, which makes it ideal as an exchange format. It achieves that as a variable multibyte encoding (up to 4 bytes) that represents more common codepoints with shorter byte sequences (ASCII, the most common in english / western languages, takes only one byte, thus an ASCII string is directly compatible to UTF-8 on byte level). Downside is a higher processing penalty for typical string actions, therefore it is less commonly seen for in-memory handling. Standard in most modern data exchanges (like WWW), recently some programming languages started to use it as internal memory represenation (e.g. Rust). UTF-8 has several advantages over other Unicode encodings like "self healing" (dropping a byte does not scramble the whole data) and not being endianess dependent.
    - [UTF-16](https://en.wikipedia.org/wiki/UTF-16)  
    This originates back to earlier Unicode specifications with only 65,535 codepoints ([UCS-2](https://en.wikipedia.org/wiki/Universal_Coded_Character_Set), 16-bit). Apparently 65,535 codepoints were not sufficiant to represent enough characters, thus the Unicode table was extended to 1,114,112 codepoints and the former UCS-2 encoding was expanded to a variable multibyte encoding, where the higher codepoints are formed by surrogate pairs. This is seen in many systems as default internal Unicode encoding (in Windows as wchar_t, internal string representation in Java and .NET), also Javascript uses this internally (with a fallback to UCS-2 in most engines). Regarding speed / memory consumption UTF-16 is a mixed case, while it can encode CJK codepoints < 65,536 more efficiently than UTF-8 (3 bytes), ASCII is more expensive and it is still quite expensive to process due to the surrogate pairs. Also its memory layout depents on endianess (UTF-16BE, UTF-16LE), is not self healing and not directly ASCII compatible (needs one padding byte). Thus it is hardly used for data exchange.
    - [UTF-32](https://en.wikipedia.org/wiki/UTF-32)  
    This encoding maps codepoints directly to 4 byte numbers in the memory, thus it is stable in terms of memory offsets and very fast for string processing. Used by many modern POSIX systems (wchar_t in C/C++). Not suitable for data exchange due to high memory consumption, endianess (UTF-32BE, UTF-32LE), no self healing and not directly ASCII compatible (needs 3 padding bytes).


## Default encoding and locale settings in operating systems

Typically operating systems offer a way to set / declare a default encoding. The system will take care that the encoding is understood by many system components (sadly not all will magically work), thus programs can expect to get meaningful string data from the system or to send meaningful content as long as they obey to the same encoding. This most basic convention is active in all relevant operating systems these days. But as always, the reality it is much more complicated. The following tries to give a simplified view on encodings regarding terminal interaction. Only the output direction is shown, yet the principles also apply to terminal input read by a program, but in the reverse direction (well mostly, the input chain is a bit simpler).

To understand how encodings influence a terminal we have to look first, how a typical console program is developed in most systems. Note that the following will apply to any programming language, still a certain language might apply different default assumptions about the internal string encoding.

Lets take simple C examples:
```C
#include <stdio.h>

int main(void) {
  printf("Hello World!\n");
  return 0;
}
```

```C
#include <wchar.h>
#include <locale.h>

int main(void) {
  setlocale(LC_ALL,"");   /* grabs system default encoding */
  wprintf(L"Ümläuts\n");  /* hopefully system encoding can represent these chars */
  return 0;
}
```

Compiled and executed the first example will most likely print "Hello World!" correctly in any terminal on any system, while the second snippet might show up correctly (congrats, your system is configured correctly for German umlauts), shows some rubbish or nothing at all.

### What happens behind the scenes?

__1. Editor encoding__

  First thing to note here is the question, what input encoding the editor applied to the string literals. Since the first snippet uses only ASCII it will work with most encodings without further problems. Not so the second example, it has to pass a first encoding obstacle - the editor itself. Most editors will derive their input encoding from system defaults, others might always use UTF-8 or allow to freely set the encoding. Try to save the second snippet to a file and re-open it with a different editor encoding to see the difference.

  *Summary*: Basically the input stage, which translates keystrokes to bytes written to disk (character to byte mapping, **input encoding**).  
  *Note*: Some editors might even separate between input and output encoding, which makes them capable of re-encoding file content.

__2. Compilation__

  Depending on the language used, the bytes of the string literals get copied over by the compiler to the executables altered or unaltered (note: the compiler has to understand the file encoding in the first place). In C the L"" marks literals as wide, thus taking more space on byte level (UTF-16 on Windows, UTF-32 on Linux, hardcoded in C-lib). On the other hand the first string is only marked as 8-bit thus can be copied directly. Both are written as byte sequences to the executables, but with different width and values. Other languages might apply here other conventions (Rust uses UTF-8), purely interpreted languages do this during their interpreter startup before execution starts. JITs also move this step to the ongoing compilation during execution.

  *Summary*: Basically a step, where bytes of the "input encoding" might get transcoded into some other encoding favored by the programming language environment.

__3. Execution__

  During execution, `printf` simply grabs the memory location of the bytes and puts everything up to the null termination into `STDOUT`. `wprintf` additionally changes `STDOUT` into wide mode and then operates the same as `printf` but with different type widths (2 byte in UTF-16, 4 in UTF-32). `setlocale` is needed to initialize the C runtime with the expected output encoding offered by the system, which turns the internal byte representation into a byte stream in the system encoding (UTF-8 on most systems). Note that other programming languages provide similar interfaces to locale settings (like `java.util.Locale` in Java). Further note that some languages hide this step from the developers and try to detect the correct output encoding on their own (e.g. Python, Node.js).

  *Summary*: A step, that transcodes from the process in-memory encoding to the appropriate **output encoding**.  
  *Note*: The locale subsystem deals with many more localization related aspects beside text encodings ([Wikipedia](https://en.wikipedia.org/wiki/Locale_(computer_software))).

__4. Terminal driver subsystem__

  The next steps the string data takes is very much operating system dependent. On POSIX systems `STDOUT` is most likely a file descriptor pointing to a writeable end of some sort of a TTY interface (e.g. a real TTY driver, PTS subsystem). Beside some additional TTY processing (see [termios manpage](http://man7.org/linux/man-pages/man3/termios.3.html)) the data is forwarded byte by byte to the reading endpoint. For Windows it works conceptually similar with the new ConPty system (though not supporting most of the TTY transformations). Older windows programs that directly spawn/attach to their console would skip this step.

  *Summary*: Does some TTY transformation on the data, does not directly impact encoding itself.

__5. Terminal / Console output__

  Finally a terminal (incl. conhost.exe) reads the incoming bytes, processes control codes or escape sequences (if supported) and mirrors printable characters to the user. To do that, the terminal decodes the bytes with its text encoding and places appropriate font glyphs on the screen. Most terminal emulators as of today operate only with Unicode (UTF-8) directly, older ones would have direct support for various encodings. Still most UTF-8 only emulators expose an encoding setting, which can be explicitly set by the user to support legacy encodings. If enabled those emulators transcode the data stream to UTF-8 before further processing takes place (often done with `luit`). Note that older windows console programs would skip the indirection of step 4 and enter data to the provided console buffer directly (basically "shortcutting" program to console access).

  *Summary*: Final step, which does the reverse mapping to output meaningful characters on the screen (byte to character mapping).

__6. Summary__

As one can see there is alot going on to get those strings shown in a terminal window with lots of assumptions. Whenever a terminal outputs scrambled data for a certain process, at least one assumption on the way to the terminal was wrong:

- wrong editor encoding  
  This was a common issue in the past for programming beginners, nowadays most editors use UTF-8 as default and every compiler / interpreter understands that.
- `setlocale` is missing or the wrong locale was applied  
  This is not a problem for pure 7-bit ASCII data, in fact this is most common in system software that only outputs English (many low level C programs never call `setlocale` explicitly). For any program dealing with non-ASCII string data this might lead to scrambled output. This is also a problem for environments that try to detect the right output encoding but get outsmarted by a complicated system configuration or unsupported output encoding. Especially in Python or Node.js developers should be prepared for this and sometimes have to fix the output encoding.
- terminal encoding differs from encoding used in `setlocale`  
  That is a quite common issue, e.g. when connecting to a remote system with `ssh` that uses a different system encoding than the terminal's host system without additional transcoding. This also can happen if a certain program deliberately sets its own encoding not compatible with the terminal's encoding (like some applications do under Windows, e.g. `git-bash` which defaults to UTF-8 regardless of the system encoding).
- terminal encoding is not capable to decode a certain codepoint  
  As written above most terminals these days are UTF-8 based, thus should understand all valid Unicode codepoints. This will still happen if the user sets the terminal encoding to a legacy encoding by purpose but guessed the encoding of the program on the other side wrong. Most terminals will show non decodeable codepoints as '?' or "�" (U+FFFD). Furthermore some font renderer might output other replacement symbols if a certain glyph is not supported.
