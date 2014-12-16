# Makefile for flymake tests

CC_OPTS = -Wall

## Recent gcc's (e.g. 4.8.2 on RHEL7) can automatically colorize their output,
## which can confuse flymake.  Set GCC_COLORS to disable that.
## TODO is this something that should be fixed in flymake?
check-syntax:
	GCC_COLORS= $(CC) $(CC_OPTS) ${CHK_SOURCES}

# eof
