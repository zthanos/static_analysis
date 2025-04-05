# DOGECOIN
---
**File**: DOGEMAIN
**Program**: DOGECOIN
**Language**: COBOL
---
## 00000-MAIN
---
**Description**: Main entry point for 00000-MAIN functionality

**Internal Calls**:
- 00000-MAIN
- DOGE-MAIN-SCREEN
- DOGE-WTO
- PARSE-OPTION
- RECEIVE-OPTION

**External Calls**:
- RETURN TRANSID('DOGE') COMMAREA(DOGECOMMS-AREA)
- SEND MAP('DOGECN1') MAPSET('DOGECN') ERASE
- XCTL PROGRAM('DOGEQUIT')

**Archimate Diagram:**
![Diagram](\svg\diagram_00000_MAIN.svg)
### Analyzed Paths
---
**Use case** (Weight: 7.7)

**Business Rules:**
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = True
```

**Execution Path:**
1. 00000-MAIN
1. Assign value to DOGECOMMS-AREA
1. Assign value to WTO-MESSAGE
1. DOGE-WTO
1. Assign value to WTO-MESSAGE
1. DOGE-WTO
1. SEND MAP('DOGECN1') MAPSET('DOGECN') ERASE
1. RETURN TRANSID('DOGE') COMMAREA(DOGECOMMS-AREA)
---
**Use case** (Weight: 7.5)

**Business Rules:**
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = True
```

**Execution Path:**
1. 00000-MAIN
1. Assign value to DOGECOMMS-AREA
1. XCTL PROGRAM('DOGEQUIT')
1. RETURN TRANSID('DOGE') COMMAREA(DOGECOMMS-AREA)
---
**Use case** (Weight: 8.4)

**Business Rules:**
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF5 = True
```

**Execution Path:**
1. 00000-MAIN
1. Assign value to DOGECOMMS-AREA
1. DOGE-MAIN-SCREEN
1. RETURN TRANSID('DOGE') COMMAREA(DOGECOMMS-AREA)
---
**Use case** (Weight: 10.1)

**Business Rules:**
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF5 = False
WOW-MENU = True
```

**Execution Path:**
1. 00000-MAIN
1. Assign value to DOGECOMMS-AREA
1. Assign value to DOGECOMMS-AREA
1. DOGE-MAIN-SCREEN
1. RETURN TRANSID('DOGE') COMMAREA(DOGECOMMS-AREA)
---
**Use case** (Weight: 11.7)

**Business Rules:**
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF5 = False
WOW-MENU = False
EIBAID EQUAL TO DFHENTER = True
```

**Execution Path:**
1. 00000-MAIN
1. Assign value to DOGECOMMS-AREA
1. RECEIVE-OPTION
1. PARSE-OPTION
1. RETURN TRANSID('DOGE') COMMAREA(DOGECOMMS-AREA)
---
**Use case** (Weight: 9.7)

**Business Rules:**
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF5 = False
WOW-MENU = False
EIBAID EQUAL TO DFHENTER = False
```

**Execution Path:**
1. 00000-MAIN
1. Assign value to DOGECOMMS-AREA
1. RETURN TRANSID('DOGE') COMMAREA(DOGECOMMS-AREA)
---
**Use case** (Weight: 6.4)

**Business Rules:**
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = True
```

**Execution Path:**
1. 00000-MAIN
1. Assign value to WTO-MESSAGE
1. DOGE-WTO
1. Assign value to WTO-MESSAGE
1. DOGE-WTO
1. SEND MAP('DOGECN1') MAPSET('DOGECN') ERASE
1. RETURN TRANSID('DOGE') COMMAREA(DOGECOMMS-AREA)
---
**Use case** (Weight: 6.2)

**Business Rules:**
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = True
```

**Execution Path:**
1. 00000-MAIN
1. XCTL PROGRAM('DOGEQUIT')
1. RETURN TRANSID('DOGE') COMMAREA(DOGECOMMS-AREA)
---
**Use case** (Weight: 7.1)

**Business Rules:**
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF5 = True
```

**Execution Path:**
1. 00000-MAIN
1. DOGE-MAIN-SCREEN
1. RETURN TRANSID('DOGE') COMMAREA(DOGECOMMS-AREA)
---
**Use case** (Weight: 8.8)

**Business Rules:**
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF5 = False
WOW-MENU = True
```

**Execution Path:**
1. 00000-MAIN
1. Assign value to DOGECOMMS-AREA
1. DOGE-MAIN-SCREEN
1. RETURN TRANSID('DOGE') COMMAREA(DOGECOMMS-AREA)
---
**Use case** (Weight: 10.4)

**Business Rules:**
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF5 = False
WOW-MENU = False
EIBAID EQUAL TO DFHENTER = True
```

**Execution Path:**
1. 00000-MAIN
1. RECEIVE-OPTION
1. PARSE-OPTION
1. RETURN TRANSID('DOGE') COMMAREA(DOGECOMMS-AREA)
---
**Use case** (Weight: 8.4)

**Business Rules:**
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF5 = False
WOW-MENU = False
EIBAID EQUAL TO DFHENTER = False
```

**Execution Path:**
1. 00000-MAIN
1. RETURN TRANSID('DOGE') COMMAREA(DOGECOMMS-AREA)


## DOGE-EXIT
---
**Description**: Main entry point for DOGE-EXIT functionality

**Internal Calls**:
- DOGE-EXIT

**External Calls**:

**Archimate Diagram:**
![Diagram](\svg\diagram_DOGE_EXIT.svg)
### Analyzed Paths
---
**Use case** (Weight: 1.5)

**Execution Path:**
1. DOGE-EXIT
1. GOBACK


## RECEIVE-OPTION
---
**Description**: Main entry point for RECEIVE-OPTION functionality

**Internal Calls**:
- DOGE-WTO
- RECEIVE-OPTION

**External Calls**:
- RECEIVE MAP('DOGEMN1') MAPSET('DOGEMN') INTO(DOGEMN1I)

**Archimate Diagram:**
![Diagram](\svg\diagram_RECEIVE_OPTION.svg)
### Analyzed Paths
---
**Use case** (Weight: 2.8)

**Execution Path:**
1. RECEIVE-OPTION
1. Assign value to WTO-MESSAGE
1. DOGE-WTO
1. RECEIVE MAP('DOGEMN1') MAPSET('DOGEMN') INTO(DOGEMN1I)


## PARSE-OPTION
---
**Description**: Main entry point for PARSE-OPTION functionality

**Internal Calls**:
- DOGE-WTO
- PARSE-OPTION

**External Calls**:
- XCTL PROGRAM('DOGECOIN')
- XCTL PROGRAM('DOGEDEET')
- XCTL PROGRAM('DOGESEND')
- XCTL PROGRAM('DOGETRAN')

**Archimate Diagram:**
![Diagram](\svg\diagram_PARSE_OPTION.svg)
### Analyzed Paths
---
**Use case** (Weight: 4.5)

**Business Rules:**
```
OPTIONI EQUAL TO 'T' = True
```

**Execution Path:**
1. PARSE-OPTION
1. Assign value to WTO-MESSAGE
1. DOGE-WTO
1. XCTL PROGRAM('DOGETRAN')
1. Assign value to WTO-MESSAGE
---
**Use case** (Weight: 5.9)

**Business Rules:**
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = True
```

**Execution Path:**
1. PARSE-OPTION
1. Assign value to WTO-MESSAGE
1. DOGE-WTO
1. XCTL PROGRAM('DOGECOIN')
1. Assign value to WTO-MESSAGE
---
**Use case** (Weight: 7.3)

**Business Rules:**
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = False
OPTIONI EQUAL TO 'D' = True
```

**Execution Path:**
1. PARSE-OPTION
1. Assign value to WTO-MESSAGE
1. DOGE-WTO
1. XCTL PROGRAM('DOGEDEET')
1. Assign value to WTO-MESSAGE
---
**Use case** (Weight: 8.7)

**Business Rules:**
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = False
OPTIONI EQUAL TO 'D' = False
OPTIONI EQUAL TO 'S' = True
```

**Execution Path:**
1. PARSE-OPTION
1. Assign value to WTO-MESSAGE
1. DOGE-WTO
1. XCTL PROGRAM('DOGESEND')
1. Assign value to WTO-MESSAGE
---
**Use case** (Weight: 5.9)

**Business Rules:**
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = False
OPTIONI EQUAL TO 'D' = False
OPTIONI EQUAL TO 'S' = False
```

**Execution Path:**
1. PARSE-OPTION
1. Assign value to WTO-MESSAGE


## DOGE-MAIN-SCREEN
---
**Description**: Main entry point for DOGE-MAIN-SCREEN functionality

**Internal Calls**:
- CONVERT-AMOUNT-TO-DISPLAY
- CONVERT-DATE
- DOGE-MAIN-SCREEN
- DOGE-WTO

**External Calls**:
- ENDBR FILE('DOGEVSAM')
- READNEXT FILE('DOGEVSAM') RIDFLD(START-RECORD-ID) INTO(TRANSACTION)
- READPREV FILE('DOGEVSAM') RIDFLD(START-RECORD-ID) INTO(TRANSACTION)
- RESETBR FILE('DOGEVSAM') RIDFLD(START-RECORD-ID)
- SEND MAP('DOGEMN1') MAPSET('DOGEMN') ERASE
- STARTBR FILE('DOGEVSAM') RIDFLD(START-RECORD-ID)

**Archimate Diagram:**
![Diagram](\svg\diagram_DOGE_MAIN_SCREEN.svg)
### Analyzed Paths
---
**Use case** (Weight: 18.8)

**Business Rules:**
```
TDATE = 0000000002 = True
```

**Execution Path:**
1. DOGE-MAIN-SCREEN
1. Assign value to WTO-MESSAGE
1. DOGE-WTO
1. STARTBR FILE('DOGEVSAM') RIDFLD(START-RECORD-ID)
1. READPREV FILE('DOGEVSAM') RIDFLD(START-RECORD-ID) INTO(TRANSACTION)
1. READPREV FILE('DOGEVSAM') RIDFLD(START-RECORD-ID) INTO(TRANSACTION)
1. CONVERT-DATE
1. Assign value to DLABEL
1. CONVERT-AMOUNT-TO-DISPLAY
1. Assign value to RECNT2C
1. Assign value to RECNT2O
1. READPREV FILE('DOGEVSAM') RIDFLD(START-RECORD-ID) INTO(TRANSACTION)
1. CONVERT-DATE
1. Assign value to DLABEL
1. CONVERT-AMOUNT-TO-DISPLAY
1. Assign value to RECNT1C
1. Assign value to RECNT1O
1. Assign value to RECNT1O
1. Assign value to START-RECORD-ID
1. RESETBR FILE('DOGEVSAM') RIDFLD(START-RECORD-ID)
1. READNEXT FILE('DOGEVSAM') RIDFLD(START-RECORD-ID) INTO(TRANSACTION)
1. CONVERT-AMOUNT-TO-DISPLAY
1. Assign value to AVAILABLE-AMOUNT
1. Assign value to AVAILO
1. READNEXT FILE('DOGEVSAM') RIDFLD(START-RECORD-ID) INTO(TRANSACTION)
1. CONVERT-AMOUNT-TO-DISPLAY
1. Assign value to PENDNGO
1. Assign value to TOTALO
1. ENDBR FILE('DOGEVSAM')
1. SEND MAP('DOGEMN1') MAPSET('DOGEMN') ERASE
---
**Use case** (Weight: 18.5)

**Business Rules:**
```
TDATE = 0000000002 = False
```

**Execution Path:**
1. DOGE-MAIN-SCREEN
1. Assign value to WTO-MESSAGE
1. DOGE-WTO
1. STARTBR FILE('DOGEVSAM') RIDFLD(START-RECORD-ID)
1. READPREV FILE('DOGEVSAM') RIDFLD(START-RECORD-ID) INTO(TRANSACTION)
1. READPREV FILE('DOGEVSAM') RIDFLD(START-RECORD-ID) INTO(TRANSACTION)
1. CONVERT-DATE
1. Assign value to DLABEL
1. CONVERT-AMOUNT-TO-DISPLAY
1. Assign value to RECNT2C
1. Assign value to RECNT2O
1. READPREV FILE('DOGEVSAM') RIDFLD(START-RECORD-ID) INTO(TRANSACTION)
1. CONVERT-DATE
1. Assign value to DLABEL
1. CONVERT-AMOUNT-TO-DISPLAY
1. Assign value to RECNT1C
1. Assign value to RECNT1O
1. Assign value to START-RECORD-ID
1. RESETBR FILE('DOGEVSAM') RIDFLD(START-RECORD-ID)
1. READNEXT FILE('DOGEVSAM') RIDFLD(START-RECORD-ID) INTO(TRANSACTION)
1. CONVERT-AMOUNT-TO-DISPLAY
1. Assign value to AVAILABLE-AMOUNT
1. Assign value to AVAILO
1. READNEXT FILE('DOGEVSAM') RIDFLD(START-RECORD-ID) INTO(TRANSACTION)
1. CONVERT-AMOUNT-TO-DISPLAY
1. Assign value to PENDNGO
1. Assign value to TOTALO
1. ENDBR FILE('DOGEVSAM')
1. SEND MAP('DOGEMN1') MAPSET('DOGEMN') ERASE


## CONVERT-AMOUNT-TO-DISPLAY
---
**Description**: Main entry point for CONVERT-AMOUNT-TO-DISPLAY functionality

**Internal Calls**:
- CONVERT-AMOUNT-TO-DISPLAY

**External Calls**:

**Archimate Diagram:**
![Diagram](\svg\diagram_CONVERT_AMOUNT_TO_DISPLAY.svg)
### Analyzed Paths
---
**Use case** (Weight: 4.2)

**Business Rules:**
```
TAMT-SIGN-NEGATIVE = True
```

**Execution Path:**
1. CONVERT-AMOUNT-TO-DISPLAY
1. Assign value to RECENT-COLOR
1. Assign value to THE-AMOUNT-INTEGER
1. Assign value to THE-AMOUNT-DECIMAL
1. Assign value to RECENT-COLOR
1. Assign value to DAMOUNT
1. Assign value to DSIGN
---
**Use case** (Weight: 2.9)

**Business Rules:**
```
TAMT-SIGN-NEGATIVE = False
```

**Execution Path:**
1. CONVERT-AMOUNT-TO-DISPLAY
1. Assign value to RECENT-COLOR
1. Assign value to THE-AMOUNT-INTEGER
1. Assign value to THE-AMOUNT-DECIMAL
1. Assign value to DAMOUNT
1. Assign value to DSIGN


## CONVERT-DATE
---
**Description**: Main entry point for CONVERT-DATE functionality

**Internal Calls**:
- CONVERT-DATE

**External Calls**:
- FORMATTIME ABSTIME(TEMP-DATE) DATESEP('/') MMDDYYYY(DDATE)

**Archimate Diagram:**
![Diagram](\svg\diagram_CONVERT_DATE.svg)
### Analyzed Paths
---
**Use case** (Weight: 2.3)

**Execution Path:**
1. CONVERT-DATE
1. Assign value to TEMP-DATE
1. FORMATTIME ABSTIME(TEMP-DATE) DATESEP('/') MMDDYYYY(DDATE)


## DOGE-WTO
---
**Description**: Main entry point for DOGE-WTO functionality

**Internal Calls**:
- DOGE-WTO

**External Calls**:
- WRITE OPERATOR TEXT(WTO-MESSAGE)

**Archimate Diagram:**
![Diagram](\svg\diagram_DOGE_WTO.svg)
### Analyzed Paths
---
**Use case** (Weight: 2.3)

**Execution Path:**
1. DOGE-WTO
1. WRITE OPERATOR TEXT(WTO-MESSAGE)
1. Assign value to WTO-MESSAGE

