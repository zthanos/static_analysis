# DOGETRAN
**File**: DOGETRAN.cbl

**Program**: DOGETRAN

**Language**: COBOL

---
## DOGE-MAIN
**Description**: Main entry point for DOGE-MAIN functionality

**Internal Calls**:
- DOGE-LIST-TRANSACTIONS
- DOGE-MAIN
- DOGE-WTO
- LET-ER-RIP
- PARSE-OPTION
- RECEIVE-OPTION

**External Calls**:
- RETURN TRANSID('DTRN') COMMAREA(DOGECOMMS-AREA)
- SEND MAP('DOGETR1') MAPSET('DOGETR') ERASE
- XCTL PROGRAM('DOGEQUIT')

**Archimate Diagram:**
![Diagram](svg/DOGETRAN_diagram_DOGE-MAIN.svg)
### Analyzed Paths
---
**Use case** (Weight: 7.9)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DOGE-MAIN_0.svg)
---
**Use case** (Weight: 9.6)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DOGE-MAIN_1.svg)
---
**Use case** (Weight: 11.2)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = False
EIBAID EQUAL TO DFHPF7 = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DOGE-MAIN_2.svg)
---
**Use case** (Weight: 10.3)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = False
EIBAID EQUAL TO DFHPF7 = False
EIBAID EQUAL TO DFHPF3 = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DOGE-MAIN_3.svg)
---
**Use case** (Weight: 11.7)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = False
EIBAID EQUAL TO DFHPF7 = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHENTER = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DOGE-MAIN_4.svg)
---
**Use case** (Weight: 9.7)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = False
EIBAID EQUAL TO DFHPF7 = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHENTER = False
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DOGE-MAIN_5.svg)
---
**Use case** (Weight: 6.6)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DOGE-MAIN_6.svg)
---
**Use case** (Weight: 8.3)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DOGE-MAIN_7.svg)
---
**Use case** (Weight: 9.9)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = False
EIBAID EQUAL TO DFHPF7 = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DOGE-MAIN_8.svg)
---
**Use case** (Weight: 9.0)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = False
EIBAID EQUAL TO DFHPF7 = False
EIBAID EQUAL TO DFHPF3 = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DOGE-MAIN_9.svg)
---
**Use case** (Weight: 10.4)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = False
EIBAID EQUAL TO DFHPF7 = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHENTER = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DOGE-MAIN_10.svg)
---
**Use case** (Weight: 8.4)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = False
EIBAID EQUAL TO DFHPF7 = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHENTER = False
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DOGE-MAIN_11.svg)


## DOGE-EXIT
**Description**: Main entry point for DOGE-EXIT functionality

**Internal Calls**:
- DOGE-EXIT

**External Calls**:

**Archimate Diagram:**
![Diagram](svg/DOGETRAN_diagram_DOGE-EXIT.svg)
### Analyzed Paths
---
**Use case** (Weight: 1.5)

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DOGE-EXIT_0.svg)


## LET-ER-RIP
**Description**: Main entry point for LET-ER-RIP functionality

**Internal Calls**:
- LET-ER-RIP

**External Calls**:
- STARTBR FILE('DOGEVSAM') RIDFLD(START-RECORD-ID)

**Archimate Diagram:**
![Diagram](svg/DOGETRAN_diagram_LET-ER-RIP.svg)
### Analyzed Paths
---
**Use case** (Weight: 2.0)

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_LET-ER-RIP_0.svg)


## BACK-IT-UP
**Description**: Main entry point for BACK-IT-UP functionality

**Internal Calls**:
- BACK-IT-UP

**External Calls**:
- READPREV FILE('DOGEVSAM') RIDFLD(START-RECORD-ID) INTO(TRANSACTION)

**Archimate Diagram:**
![Diagram](svg/DOGETRAN_diagram_BACK-IT-UP.svg)
### Analyzed Paths
---
**Use case** (Weight: 4.0)

## Business Rules:
```
START-RECORD-ID NOT EQUAL TO '0000000002' = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_BACK-IT-UP_0.svg)
---
**Use case** (Weight: 3.3)

## Business Rules:
```
START-RECORD-ID NOT EQUAL TO '0000000002' = False
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_BACK-IT-UP_1.svg)


## DOGE-LIST-TRANSACTIONS
**Description**: Main entry point for DOGE-LIST-TRANSACTIONS functionality

**Internal Calls**:
- DOGE-LIST-TRANSACTIONS

**External Calls**:
- ENDBR FILE('DOGEVSAM')
- READNEXT FILE('DOGEVSAM') RIDFLD(START-RECORD-ID) INTO(TRANSACTION)

**Archimate Diagram:**
![Diagram](svg/DOGETRAN_diagram_DOGE-LIST-TRANSACTIONS.svg)
### Analyzed Paths
---
**Use case** (Weight: 5.5)

## Business Rules:
```
DONE-RECORDS IS NOT EQUAL TO 'DONE' = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DOGE-LIST-TRANSACTIONS_0.svg)
---
**Use case** (Weight: 4.2)

## Business Rules:
```
DONE-RECORDS IS NOT EQUAL TO 'DONE' = False
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DOGE-LIST-TRANSACTIONS_1.svg)


## CONVERT-AMOUNT-TO-DISPLAY
**Description**: Main entry point for CONVERT-AMOUNT-TO-DISPLAY functionality

**Internal Calls**:
- CONVERT-AMOUNT-TO-DISPLAY

**External Calls**:

**Archimate Diagram:**
![Diagram](svg/DOGETRAN_diagram_CONVERT-AMOUNT-TO-DISPLAY.svg)
### Analyzed Paths
---
**Use case** (Weight: 4.8)

## Business Rules:
```
TAMT-SIGN-NEGATIVE = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_CONVERT-AMOUNT-TO-DISPLAY_0.svg)
---
**Use case** (Weight: 3.2)

## Business Rules:
```
TAMT-SIGN-NEGATIVE = False
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_CONVERT-AMOUNT-TO-DISPLAY_1.svg)


## CONVERT-DATE
**Description**: Main entry point for CONVERT-DATE functionality

**Internal Calls**:
- CONVERT-DATE

**External Calls**:
- FORMATTIME ABSTIME(TEMP-DATE) DATESEP('/') MMDDYYYY(DDATE)

**Archimate Diagram:**
![Diagram](svg/DOGETRAN_diagram_CONVERT-DATE.svg)
### Analyzed Paths
---
**Use case** (Weight: 2.3)

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_CONVERT-DATE_0.svg)


## DOGE-WTO
**Description**: Main entry point for DOGE-WTO functionality

**Internal Calls**:
- DOGE-WTO

**External Calls**:
- WRITE OPERATOR TEXT(WTO-MESSAGE)

**Archimate Diagram:**
![Diagram](svg/DOGETRAN_diagram_DOGE-WTO.svg)
### Analyzed Paths
---
**Use case** (Weight: 2.3)

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DOGE-WTO_0.svg)


## DISPLAY-TRANS
**Description**: Main entry point for DISPLAY-TRANS functionality

**Internal Calls**:
- CONVERT-AMOUNT-TO-DISPLAY
- CONVERT-DATE
- DISPLAY-TRANS
- FILL-ROWS-WITH-DATA

**External Calls**:
- READNEXT FILE('DOGEVSAM') RIDFLD(START-RECORD-ID) INTO(TRANSACTION)

**Archimate Diagram:**
![Diagram](svg/DOGETRAN_diagram_DISPLAY-TRANS.svg)
### Analyzed Paths
---
**Use case** (Weight: 4.6)

## Business Rules:
```
TDATE IS EQUAL TO '9999999999' = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DISPLAY-TRANS_0.svg)
---
**Use case** (Weight: 5.5)

## Business Rules:
```
TDATE IS EQUAL TO '9999999999' = False
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_DISPLAY-TRANS_1.svg)


## FILL-ROWS-WITH-DATA
**Description**: Main entry point for FILL-ROWS-WITH-DATA functionality

**Internal Calls**:
- FILL-ROWS-WITH-DATA

**External Calls**:

**Archimate Diagram:**
![Diagram](svg/DOGETRAN_diagram_FILL-ROWS-WITH-DATA.svg)
### Analyzed Paths
---
**Use case** (Weight: 5.1)

## Business Rules:
```
LINE-NUMBER IS EQUAL TO 1 = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_FILL-ROWS-WITH-DATA_0.svg)
---
**Use case** (Weight: 6.5)

## Business Rules:
```
LINE-NUMBER IS EQUAL TO 1 = False
LINE-NUMBER IS EQUAL TO 2 = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_FILL-ROWS-WITH-DATA_1.svg)
---
**Use case** (Weight: 7.9)

## Business Rules:
```
LINE-NUMBER IS EQUAL TO 1 = False
LINE-NUMBER IS EQUAL TO 2 = False
LINE-NUMBER IS EQUAL TO 3 = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_FILL-ROWS-WITH-DATA_2.svg)
---
**Use case** (Weight: 9.3)

## Business Rules:
```
LINE-NUMBER IS EQUAL TO 1 = False
LINE-NUMBER IS EQUAL TO 2 = False
LINE-NUMBER IS EQUAL TO 3 = False
LINE-NUMBER IS EQUAL TO 4 = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_FILL-ROWS-WITH-DATA_3.svg)
---
**Use case** (Weight: 10.7)

## Business Rules:
```
LINE-NUMBER IS EQUAL TO 1 = False
LINE-NUMBER IS EQUAL TO 2 = False
LINE-NUMBER IS EQUAL TO 3 = False
LINE-NUMBER IS EQUAL TO 4 = False
LINE-NUMBER IS EQUAL TO 5 = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_FILL-ROWS-WITH-DATA_4.svg)
---
**Use case** (Weight: 12.1)

## Business Rules:
```
LINE-NUMBER IS EQUAL TO 1 = False
LINE-NUMBER IS EQUAL TO 2 = False
LINE-NUMBER IS EQUAL TO 3 = False
LINE-NUMBER IS EQUAL TO 4 = False
LINE-NUMBER IS EQUAL TO 5 = False
LINE-NUMBER IS EQUAL TO 6 = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_FILL-ROWS-WITH-DATA_5.svg)
---
**Use case** (Weight: 13.5)

## Business Rules:
```
LINE-NUMBER IS EQUAL TO 1 = False
LINE-NUMBER IS EQUAL TO 2 = False
LINE-NUMBER IS EQUAL TO 3 = False
LINE-NUMBER IS EQUAL TO 4 = False
LINE-NUMBER IS EQUAL TO 5 = False
LINE-NUMBER IS EQUAL TO 6 = False
LINE-NUMBER IS EQUAL TO 7 = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_FILL-ROWS-WITH-DATA_6.svg)
---
**Use case** (Weight: 10.1)

## Business Rules:
```
LINE-NUMBER IS EQUAL TO 1 = False
LINE-NUMBER IS EQUAL TO 2 = False
LINE-NUMBER IS EQUAL TO 3 = False
LINE-NUMBER IS EQUAL TO 4 = False
LINE-NUMBER IS EQUAL TO 5 = False
LINE-NUMBER IS EQUAL TO 6 = False
LINE-NUMBER IS EQUAL TO 7 = False
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_FILL-ROWS-WITH-DATA_7.svg)


## RECEIVE-OPTION
**Description**: Main entry point for RECEIVE-OPTION functionality

**Internal Calls**:
- DOGE-WTO
- RECEIVE-OPTION

**External Calls**:
- RECEIVE MAP('DOGETR1') MAPSET('DOGETR') INTO(DOGETR1I)

**Archimate Diagram:**
![Diagram](svg/DOGETRAN_diagram_RECEIVE-OPTION.svg)
### Analyzed Paths
---
**Use case** (Weight: 2.8)

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_RECEIVE-OPTION_0.svg)


## PARSE-OPTION
**Description**: Main entry point for PARSE-OPTION functionality

**Internal Calls**:
- DOGE-WTO
- PARSE-OPTION

**External Calls**:
- XCTL PROGRAM('DOGECOIN') COMMAREA(DOGECOMMS-AREA)
- XCTL PROGRAM('DOGEDEET')
- XCTL PROGRAM('DOGESEND')

**Archimate Diagram:**
![Diagram](svg/DOGETRAN_diagram_PARSE-OPTION.svg)
### Analyzed Paths
---
**Use case** (Weight: 4.3)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_PARSE-OPTION_0.svg)
---
**Use case** (Weight: 7.0)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_PARSE-OPTION_1.svg)
---
**Use case** (Weight: 8.1)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = False
OPTIONI EQUAL TO 'D' = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_PARSE-OPTION_2.svg)
---
**Use case** (Weight: 9.5)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = False
OPTIONI EQUAL TO 'D' = False
OPTIONI EQUAL TO 'S' = True
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_PARSE-OPTION_3.svg)
---
**Use case** (Weight: 6.7)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = False
OPTIONI EQUAL TO 'D' = False
OPTIONI EQUAL TO 'S' = False
```

## Execution Path
![Diagram](svg/DOGETRAN_sl_diagram_PARSE-OPTION_4.svg)

