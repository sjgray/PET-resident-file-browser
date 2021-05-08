PET/CBM Resident File Browser (C)2021 Steve J. Gray
============================= Started: March 25, 2021

Currently "Resident File Browser" is only a description and
"SteveBrowse" is it's temporary code-name until a better name
can be thought of.

This is a work-in progress File Browser with the goal for it
to run from ROM ($9xxx or $Axxx) or perhaps Edit-ROM. It should
also eventually run as a loadable PRG file if desired.

It is written completely in 6502 assembly language using the
ACME assembler. A make file is supplied. The initial version
will be assembled to $A000 for simple activation with:

sys40960

I have been testing it using VICE.

Each day I worked on it I saved backups of the progress and
will add them to the git so you can look at the changes as
the program evolved and is evolving. It is currently in a
mostly-working state but does not have all the features I've
envisioned written yet. Comments/Feedback are welcome.

Steve