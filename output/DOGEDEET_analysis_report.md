# DOGEDEET
**File**: DOGEDEET.cbl

**Program**: DOGEDEET

**Language**: COBOL

---
# Table of Contents
- [DOGE-MAIN](#doge-main)
  - [Use case 1 (Weight: 6.9)](#doge-main-use-case-1)
  - [Use case 2 (Weight: 9.6)](#doge-main-use-case-2)
  - [Use case 3 (Weight: 10.5)](#doge-main-use-case-3)
  - [Use case 4 (Weight: 10.3)](#doge-main-use-case-4)
  - [Use case 5 (Weight: 12.5)](#doge-main-use-case-5)
  - [Use case 6 (Weight: 14.1)](#doge-main-use-case-6)
  - [Use case 7 (Weight: 11.1)](#doge-main-use-case-7)
  - [Use case 8 (Weight: 5.6)](#doge-main-use-case-8)
  - [Use case 9 (Weight: 8.3)](#doge-main-use-case-9)
  - [Use case 10 (Weight: 9.2)](#doge-main-use-case-10)
  - [Use case 11 (Weight: 9.0)](#doge-main-use-case-11)
  - [Use case 12 (Weight: 11.2)](#doge-main-use-case-12)
  - [Use case 13 (Weight: 12.8)](#doge-main-use-case-13)
  - [Use case 14 (Weight: 9.8)](#doge-main-use-case-14)
- [DOGE-EXIT](#doge-exit)
  - [Use case 1 (Weight: 1.5)](#doge-exit-use-case-1)
- [DOGE-WTO](#doge-wto)
  - [Use case 1 (Weight: 2.3)](#doge-wto-use-case-1)
- [DOGE-START-BROWSE](#doge-start-browse)
  - [Use case 1 (Weight: 5.7)](#doge-start-browse-use-case-1)
  - [Use case 2 (Weight: 4.4)](#doge-start-browse-use-case-2)
  - [Use case 3 (Weight: 5.7)](#doge-start-browse-use-case-3)
  - [Use case 4 (Weight: 4.4)](#doge-start-browse-use-case-4)
- [DOGE-SHOW-TRANSACTION](#doge-show-transaction)
  - [Use case 1 (Weight: 9.9)](#doge-show-transaction-use-case-1)
  - [Use case 2 (Weight: 8.7)](#doge-show-transaction-use-case-2)
  - [Use case 3 (Weight: 8.6)](#doge-show-transaction-use-case-3)
  - [Use case 4 (Weight: 7.4)](#doge-show-transaction-use-case-4)
  - [Use case 5 (Weight: 11.3)](#doge-show-transaction-use-case-5)
  - [Use case 6 (Weight: 10.1)](#doge-show-transaction-use-case-6)
  - [Use case 7 (Weight: 10.4)](#doge-show-transaction-use-case-7)
  - [Use case 8 (Weight: 7.2)](#doge-show-transaction-use-case-8)
- [FILL-SCREEN-DATA](#fill-screen-data)
  - [Use case 1 (Weight: 4.5)](#fill-screen-data-use-case-1)
  - [Use case 2 (Weight: 2.9)](#fill-screen-data-use-case-2)
- [CONVERT-AMOUNT-TO-DISPLAY](#convert-amount-to-display)
  - [Use case 1 (Weight: 4.5)](#convert-amount-to-display-use-case-1)
  - [Use case 2 (Weight: 3.2)](#convert-amount-to-display-use-case-2)
- [CONVERT-DATE](#convert-date)
  - [Use case 1 (Weight: 3.3)](#convert-date-use-case-1)
- [RECEIVE-KEY](#receive-key)
  - [Use case 1 (Weight: 2.0)](#receive-key-use-case-1)
- [PARSE-KEY](#parse-key)
  - [Use case 1 (Weight: 7.1)](#parse-key-use-case-1)
  - [Use case 2 (Weight: 8.8)](#parse-key-use-case-2)
  - [Use case 3 (Weight: 9.9)](#parse-key-use-case-3)
  - [Use case 4 (Weight: 8.7)](#parse-key-use-case-4)
---
<a name="doge-main"></a>
## DOGE-MAIN
**Description**: Main entry point for DOGE-MAIN functionality

**Internal Calls**:
- DOGE-MAIN
- DOGE-SHOW-TRANSACTION
- DOGE-START-BROWSE
- DOGE-WTO
- PARSE-KEY
- RECEIVE-KEY

**External Calls**:
- RETURN TRANSID('DEET') COMMAREA(DOGECOMMS-AREA)
- SEND MAP('DOGEDT1') MAPSET('DOGEDT') ERASE
- XCTL PROGRAM('DOGEQUIT')
- XCTL PROGRAM('DOGETRAN')

**Archimate Diagram:**
![Diagram](svg/DOGEDEET_diagram_DOGE-MAIN.svg)
### Analyzed Paths
---
<a name="doge-main-use-case-1"></a>
**Use case** (Weight: 6.9)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-MAIN_0.svg)
---
<a name="doge-main-use-case-2"></a>
**Use case** (Weight: 9.6)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-MAIN_1.svg)
---
<a name="doge-main-use-case-3"></a>
**Use case** (Weight: 10.5)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = False
EIBAID EQUAL TO DFHPF7 = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-MAIN_2.svg)
---
<a name="doge-main-use-case-4"></a>
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
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-MAIN_3.svg)
---
<a name="doge-main-use-case-5"></a>
**Use case** (Weight: 12.5)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = False
EIBAID EQUAL TO DFHPF7 = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF6 = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-MAIN_4.svg)
---
<a name="doge-main-use-case-6"></a>
**Use case** (Weight: 14.1)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = False
EIBAID EQUAL TO DFHPF7 = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF6 = False
EIBAID EQUAL TO DFHENTER = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-MAIN_5.svg)
---
<a name="doge-main-use-case-7"></a>
**Use case** (Weight: 11.1)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = False
EIBAID EQUAL TO DFHPF7 = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF6 = False
EIBAID EQUAL TO DFHENTER = False
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-MAIN_6.svg)
---
<a name="doge-main-use-case-8"></a>
**Use case** (Weight: 5.6)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-MAIN_7.svg)
---
<a name="doge-main-use-case-9"></a>
**Use case** (Weight: 8.3)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-MAIN_8.svg)
---
<a name="doge-main-use-case-10"></a>
**Use case** (Weight: 9.2)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = False
EIBAID EQUAL TO DFHPF7 = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-MAIN_9.svg)
---
<a name="doge-main-use-case-11"></a>
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
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-MAIN_10.svg)
---
<a name="doge-main-use-case-12"></a>
**Use case** (Weight: 11.2)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = False
EIBAID EQUAL TO DFHPF7 = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF6 = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-MAIN_11.svg)
---
<a name="doge-main-use-case-13"></a>
**Use case** (Weight: 12.8)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = False
EIBAID EQUAL TO DFHPF7 = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF6 = False
EIBAID EQUAL TO DFHENTER = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-MAIN_12.svg)
---
<a name="doge-main-use-case-14"></a>
**Use case** (Weight: 9.8)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF8 = False
EIBAID EQUAL TO DFHPF7 = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF6 = False
EIBAID EQUAL TO DFHENTER = False
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-MAIN_13.svg)


<a name="doge-exit"></a>
## DOGE-EXIT
**Description**: Main entry point for DOGE-EXIT functionality

**Internal Calls**:
- DOGE-EXIT

**External Calls**:

**Archimate Diagram:**
![Diagram](svg/DOGEDEET_diagram_DOGE-EXIT.svg)
### Analyzed Paths
---
<a name="doge-exit-use-case-1"></a>
**Use case** (Weight: 1.5)

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-EXIT_0.svg)


<a name="doge-wto"></a>
## DOGE-WTO
**Description**: Main entry point for DOGE-WTO functionality

**Internal Calls**:
- DOGE-WTO

**External Calls**:
- WRITE OPERATOR TEXT(WTO-MESSAGE)

**Archimate Diagram:**
![Diagram](svg/DOGEDEET_diagram_DOGE-WTO.svg)
### Analyzed Paths
---
<a name="doge-wto-use-case-1"></a>
**Use case** (Weight: 2.3)

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-WTO_0.svg)


<a name="doge-start-browse"></a>
## DOGE-START-BROWSE
**Description**: Main entry point for DOGE-START-BROWSE functionality

**Internal Calls**:
- DOGE-START-BROWSE

**External Calls**:
- STARTBR FILE('DOGEVSAM') RIDFLD(RECORD-ID) EQUAL RESP(RESPONSE-CODE)

**Archimate Diagram:**
![Diagram](svg/DOGEDEET_diagram_DOGE-START-BROWSE.svg)
### Analyzed Paths
---
<a name="doge-start-browse-use-case-1"></a>
**Use case** (Weight: 5.7)

## Business Rules:
```
RESPONSE-CODE IS EQUAL TO DFHRESP(NOTFND) = True
RECORD-ID IS EQUAL TO 0000000001 = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-START-BROWSE_0.svg)
---
<a name="doge-start-browse-use-case-2"></a>
**Use case** (Weight: 4.4)

## Business Rules:
```
RESPONSE-CODE IS EQUAL TO DFHRESP(NOTFND) = True
RECORD-ID IS EQUAL TO 0000000001 = False
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-START-BROWSE_1.svg)
---
<a name="doge-start-browse-use-case-3"></a>
**Use case** (Weight: 5.7)

## Business Rules:
```
RESPONSE-CODE IS EQUAL TO DFHRESP(NOTFND) = False
RECORD-ID IS EQUAL TO 0000000001 = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-START-BROWSE_2.svg)
---
<a name="doge-start-browse-use-case-4"></a>
**Use case** (Weight: 4.4)

## Business Rules:
```
RESPONSE-CODE IS EQUAL TO DFHRESP(NOTFND) = False
RECORD-ID IS EQUAL TO 0000000001 = False
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-START-BROWSE_3.svg)


<a name="doge-show-transaction"></a>
## DOGE-SHOW-TRANSACTION
**Description**: Main entry point for DOGE-SHOW-TRANSACTION functionality

**Internal Calls**:
- CONVERT-AMOUNT-TO-DISPLAY
- CONVERT-DATE
- DOGE-SHOW-TRANSACTION
- FILL-SCREEN-DATA

**External Calls**:
- READNEXT FILE('DOGEVSAM') RIDFLD(RECORD-ID) INTO(TRANSACTION)
- READPREV FILE('DOGEVSAM') RIDFLD(RECORD-ID) INTO(TRANSACTION)

**Archimate Diagram:**
![Diagram](svg/DOGEDEET_diagram_DOGE-SHOW-TRANSACTION.svg)
### Analyzed Paths
---
<a name="doge-show-transaction-use-case-1"></a>
**Use case** (Weight: 9.9)

## Business Rules:
```
WE-GOT-ITANDBACKWARD = True
RECORD-ID EQUAL TO 0000000002 = True
WE-GOT-IT = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-SHOW-TRANSACTION_0.svg)
---
<a name="doge-show-transaction-use-case-2"></a>
**Use case** (Weight: 8.7)

## Business Rules:
```
WE-GOT-ITANDBACKWARD = True
RECORD-ID EQUAL TO 0000000002 = True
WE-GOT-IT = False
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-SHOW-TRANSACTION_1.svg)
---
<a name="doge-show-transaction-use-case-3"></a>
**Use case** (Weight: 8.6)

## Business Rules:
```
WE-GOT-ITANDBACKWARD = True
RECORD-ID EQUAL TO 0000000002 = False
WE-GOT-IT = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-SHOW-TRANSACTION_2.svg)
---
<a name="doge-show-transaction-use-case-4"></a>
**Use case** (Weight: 7.4)

## Business Rules:
```
WE-GOT-ITANDBACKWARD = True
RECORD-ID EQUAL TO 0000000002 = False
WE-GOT-IT = False
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-SHOW-TRANSACTION_3.svg)
---
<a name="doge-show-transaction-use-case-5"></a>
**Use case** (Weight: 11.3)

## Business Rules:
```
WE-GOT-ITANDBACKWARD = False
WE-GOT-ITANDFORWARD = True
RECORD-ID EQUAL TO 0000000002 = True
WE-GOT-IT = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-SHOW-TRANSACTION_4.svg)
---
<a name="doge-show-transaction-use-case-6"></a>
**Use case** (Weight: 10.1)

## Business Rules:
```
WE-GOT-ITANDBACKWARD = False
WE-GOT-ITANDFORWARD = True
RECORD-ID EQUAL TO 0000000002 = True
WE-GOT-IT = False
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-SHOW-TRANSACTION_5.svg)
---
<a name="doge-show-transaction-use-case-7"></a>
**Use case** (Weight: 10.4)

## Business Rules:
```
WE-GOT-ITANDBACKWARD = False
WE-GOT-ITANDFORWARD = False
WE-GOT-IT = True
RECORD-ID EQUAL TO 0000000002 = False
WE-GOT-IT = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-SHOW-TRANSACTION_6.svg)
---
<a name="doge-show-transaction-use-case-8"></a>
**Use case** (Weight: 7.2)

## Business Rules:
```
WE-GOT-ITANDBACKWARD = False
WE-GOT-ITANDFORWARD = False
WE-GOT-IT = False
RECORD-ID EQUAL TO 0000000002 = False
WE-GOT-IT = False
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_DOGE-SHOW-TRANSACTION_7.svg)


<a name="fill-screen-data"></a>
## FILL-SCREEN-DATA
**Description**: Main entry point for FILL-SCREEN-DATA functionality

**Internal Calls**:
- FILL-SCREEN-DATA

**External Calls**:

**Archimate Diagram:**
![Diagram](svg/DOGEDEET_diagram_FILL-SCREEN-DATA.svg)
### Analyzed Paths
---
<a name="fill-screen-data-use-case-1"></a>
**Use case** (Weight: 4.5)

## Business Rules:
```
TAMT-SIGN-NEGATIVE = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_FILL-SCREEN-DATA_0.svg)
---
<a name="fill-screen-data-use-case-2"></a>
**Use case** (Weight: 2.9)

## Business Rules:
```
TAMT-SIGN-NEGATIVE = False
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_FILL-SCREEN-DATA_1.svg)


<a name="convert-amount-to-display"></a>
## CONVERT-AMOUNT-TO-DISPLAY
**Description**: Main entry point for CONVERT-AMOUNT-TO-DISPLAY functionality

**Internal Calls**:
- CONVERT-AMOUNT-TO-DISPLAY

**External Calls**:

**Archimate Diagram:**
![Diagram](svg/DOGEDEET_diagram_CONVERT-AMOUNT-TO-DISPLAY.svg)
### Analyzed Paths
---
<a name="convert-amount-to-display-use-case-1"></a>
**Use case** (Weight: 4.5)

## Business Rules:
```
TAMT-SIGN-NEGATIVE = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_CONVERT-AMOUNT-TO-DISPLAY_0.svg)
---
<a name="convert-amount-to-display-use-case-2"></a>
**Use case** (Weight: 3.2)

## Business Rules:
```
TAMT-SIGN-NEGATIVE = False
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_CONVERT-AMOUNT-TO-DISPLAY_1.svg)


<a name="convert-date"></a>
## CONVERT-DATE
**Description**: Main entry point for CONVERT-DATE functionality

**Internal Calls**:
- CONVERT-DATE

**External Calls**:
- FORMATTIME ABSTIME(TEMP-DATE) DATESEP('/') MMDDYYYY(FDATE)
- FORMATTIME ABSTIME(TEMP-DATE) TIMESEP(':') TIME(FTIME)

**Archimate Diagram:**
![Diagram](svg/DOGEDEET_diagram_CONVERT-DATE.svg)
### Analyzed Paths
---
<a name="convert-date-use-case-1"></a>
**Use case** (Weight: 3.3)

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_CONVERT-DATE_0.svg)


<a name="receive-key"></a>
## RECEIVE-KEY
**Description**: Main entry point for RECEIVE-KEY functionality

**Internal Calls**:
- RECEIVE-KEY

**External Calls**:
- RECEIVE MAP('DOGEDT1') MAPSET('DOGEDT') INTO(DOGEDT1I) ASIS

**Archimate Diagram:**
![Diagram](svg/DOGEDEET_diagram_RECEIVE-KEY.svg)
### Analyzed Paths
---
<a name="receive-key-use-case-1"></a>
**Use case** (Weight: 2.0)

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_RECEIVE-KEY_0.svg)


<a name="parse-key"></a>
## PARSE-KEY
**Description**: Main entry point for PARSE-KEY functionality

**Internal Calls**:
- DOGE-SHOW-TRANSACTION
- DOGE-START-BROWSE
- DOGE-WTO
- PARSE-KEY

**External Calls**:
- XCTL PROGRAM('DOGECOIN') COMMAREA(DOGECOMMS-AREA)
- XCTL PROGRAM('DOGESEND')
- XCTL PROGRAM('DOGETRAN')

**Archimate Diagram:**
![Diagram](svg/DOGEDEET_diagram_PARSE-KEY.svg)
### Analyzed Paths
---
<a name="parse-key-use-case-1"></a>
**Use case** (Weight: 7.1)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_PARSE-KEY_0.svg)
---
<a name="parse-key-use-case-2"></a>
**Use case** (Weight: 8.8)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_PARSE-KEY_1.svg)
---
<a name="parse-key-use-case-3"></a>
**Use case** (Weight: 9.9)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = False
OPTIONI EQUAL TO 'S' = True
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_PARSE-KEY_2.svg)
---
<a name="parse-key-use-case-4"></a>
**Use case** (Weight: 8.7)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = False
OPTIONI EQUAL TO 'S' = False
```

## Execution Path
![Diagram](svg/DOGEDEET_sl_diagram_PARSE-KEY_3.svg)

