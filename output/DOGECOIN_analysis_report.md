# DOGECOIN
---
**File**: DOGEMAIN.cbl
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
![Diagram](svg/DOGECOIN_diagram_00000-MAIN.svg)
### Analyzed Paths
---
**Use case** (Weight: 7.7)

**Business Rules:**
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = True
```

**Execution Path:**
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_0.svg)
---
**Use case** (Weight: 7.5)

**Business Rules:**
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = True
```

**Execution Path:**
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_1.svg)
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
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_2.svg)
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
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_3.svg)
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
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_4.svg)
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
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_5.svg)
---
**Use case** (Weight: 6.4)

**Business Rules:**
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = True
```

**Execution Path:**
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_6.svg)
---
**Use case** (Weight: 6.2)

**Business Rules:**
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = True
```

**Execution Path:**
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_7.svg)
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
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_8.svg)
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
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_9.svg)
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
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_10.svg)
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
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_11.svg)


## DOGE-EXIT
---
**Description**: Main entry point for DOGE-EXIT functionality

**Internal Calls**:
- DOGE-EXIT

**External Calls**:

**Archimate Diagram:**
![Diagram](svg/DOGECOIN_diagram_DOGE-EXIT.svg)
### Analyzed Paths
---
**Use case** (Weight: 1.5)

**Execution Path:**
![Diagram](svg/DOGECOIN_sl_diagram_DOGE-EXIT_0.svg)


## RECEIVE-OPTION
---
**Description**: Main entry point for RECEIVE-OPTION functionality

**Internal Calls**:
- DOGE-WTO
- RECEIVE-OPTION

**External Calls**:
- RECEIVE MAP('DOGEMN1') MAPSET('DOGEMN') INTO(DOGEMN1I)

**Archimate Diagram:**
![Diagram](svg/DOGECOIN_diagram_RECEIVE-OPTION.svg)
### Analyzed Paths
---
**Use case** (Weight: 2.8)

**Execution Path:**
![Diagram](svg/DOGECOIN_sl_diagram_RECEIVE-OPTION_0.svg)


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
![Diagram](svg/DOGECOIN_diagram_PARSE-OPTION.svg)
### Analyzed Paths
---
**Use case** (Weight: 4.5)

**Business Rules:**
```
OPTIONI EQUAL TO 'T' = True
```

**Execution Path:**
![Diagram](svg/DOGECOIN_sl_diagram_PARSE-OPTION_0.svg)
---
**Use case** (Weight: 5.9)

**Business Rules:**
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = True
```

**Execution Path:**
![Diagram](svg/DOGECOIN_sl_diagram_PARSE-OPTION_1.svg)
---
**Use case** (Weight: 7.3)

**Business Rules:**
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = False
OPTIONI EQUAL TO 'D' = True
```

**Execution Path:**
![Diagram](svg/DOGECOIN_sl_diagram_PARSE-OPTION_2.svg)
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
![Diagram](svg/DOGECOIN_sl_diagram_PARSE-OPTION_3.svg)
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
![Diagram](svg/DOGECOIN_sl_diagram_PARSE-OPTION_4.svg)


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
![Diagram](svg/DOGECOIN_diagram_DOGE-MAIN-SCREEN.svg)
### Analyzed Paths
---
**Use case** (Weight: 18.8)

**Business Rules:**
```
TDATE = 0000000002 = True
```

**Execution Path:**
![Diagram](svg/DOGECOIN_sl_diagram_DOGE-MAIN-SCREEN_0.svg)
---
**Use case** (Weight: 18.5)

**Business Rules:**
```
TDATE = 0000000002 = False
```

**Execution Path:**
![Diagram](svg/DOGECOIN_sl_diagram_DOGE-MAIN-SCREEN_1.svg)


## CONVERT-AMOUNT-TO-DISPLAY
---
**Description**: Main entry point for CONVERT-AMOUNT-TO-DISPLAY functionality

**Internal Calls**:
- CONVERT-AMOUNT-TO-DISPLAY

**External Calls**:

**Archimate Diagram:**
![Diagram](svg/DOGECOIN_diagram_CONVERT-AMOUNT-TO-DISPLAY.svg)
### Analyzed Paths
---
**Use case** (Weight: 4.2)

**Business Rules:**
```
TAMT-SIGN-NEGATIVE = True
```

**Execution Path:**
![Diagram](svg/DOGECOIN_sl_diagram_CONVERT-AMOUNT-TO-DISPLAY_0.svg)
---
**Use case** (Weight: 2.9)

**Business Rules:**
```
TAMT-SIGN-NEGATIVE = False
```

**Execution Path:**
![Diagram](svg/DOGECOIN_sl_diagram_CONVERT-AMOUNT-TO-DISPLAY_1.svg)


## CONVERT-DATE
---
**Description**: Main entry point for CONVERT-DATE functionality

**Internal Calls**:
- CONVERT-DATE

**External Calls**:
- FORMATTIME ABSTIME(TEMP-DATE) DATESEP('/') MMDDYYYY(DDATE)

**Archimate Diagram:**
![Diagram](svg/DOGECOIN_diagram_CONVERT-DATE.svg)
### Analyzed Paths
---
**Use case** (Weight: 2.3)

**Execution Path:**
![Diagram](svg/DOGECOIN_sl_diagram_CONVERT-DATE_0.svg)


## DOGE-WTO
---
**Description**: Main entry point for DOGE-WTO functionality

**Internal Calls**:
- DOGE-WTO

**External Calls**:
- WRITE OPERATOR TEXT(WTO-MESSAGE)

**Archimate Diagram:**
![Diagram](svg/DOGECOIN_diagram_DOGE-WTO.svg)
### Analyzed Paths
---
**Use case** (Weight: 2.3)

**Execution Path:**
![Diagram](svg/DOGECOIN_sl_diagram_DOGE-WTO_0.svg)

