# DOGECOIN
**File**: DOGEMAIN.cbl

**Program**: DOGECOIN

**Language**: COBOL

---
# Table of Contents
- [00000-MAIN](#00000-main)
  - [Use case 1 (Weight: 7.7)](#00000-main-use-case-1)
  - [Use case 2 (Weight: 7.5)](#00000-main-use-case-2)
  - [Use case 3 (Weight: 8.4)](#00000-main-use-case-3)
  - [Use case 4 (Weight: 10.1)](#00000-main-use-case-4)
  - [Use case 5 (Weight: 11.7)](#00000-main-use-case-5)
  - [Use case 6 (Weight: 9.7)](#00000-main-use-case-6)
  - [Use case 7 (Weight: 6.4)](#00000-main-use-case-7)
  - [Use case 8 (Weight: 6.2)](#00000-main-use-case-8)
  - [Use case 9 (Weight: 7.1)](#00000-main-use-case-9)
  - [Use case 10 (Weight: 8.8)](#00000-main-use-case-10)
  - [Use case 11 (Weight: 10.4)](#00000-main-use-case-11)
  - [Use case 12 (Weight: 8.4)](#00000-main-use-case-12)
- [DOGE-EXIT](#doge-exit)
  - [Use case 1 (Weight: 1.5)](#doge-exit-use-case-1)
- [RECEIVE-OPTION](#receive-option)
  - [Use case 1 (Weight: 2.8)](#receive-option-use-case-1)
- [PARSE-OPTION](#parse-option)
  - [Use case 1 (Weight: 4.5)](#parse-option-use-case-1)
  - [Use case 2 (Weight: 5.9)](#parse-option-use-case-2)
  - [Use case 3 (Weight: 7.3)](#parse-option-use-case-3)
  - [Use case 4 (Weight: 8.7)](#parse-option-use-case-4)
  - [Use case 5 (Weight: 5.9)](#parse-option-use-case-5)
- [DOGE-MAIN-SCREEN](#doge-main-screen)
  - [Use case 1 (Weight: 18.8)](#doge-main-screen-use-case-1)
  - [Use case 2 (Weight: 18.5)](#doge-main-screen-use-case-2)
- [CONVERT-AMOUNT-TO-DISPLAY](#convert-amount-to-display)
  - [Use case 1 (Weight: 4.2)](#convert-amount-to-display-use-case-1)
  - [Use case 2 (Weight: 2.9)](#convert-amount-to-display-use-case-2)
- [CONVERT-DATE](#convert-date)
  - [Use case 1 (Weight: 2.3)](#convert-date-use-case-1)
- [DOGE-WTO](#doge-wto)
  - [Use case 1 (Weight: 2.3)](#doge-wto-use-case-1)
---
<a name="00000-main"></a>
## 00000-MAIN
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
<a name="00000-main-use-case-1"></a>
**Use case** (Weight: 7.7)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = True
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_0.svg)
---
<a name="00000-main-use-case-2"></a>
**Use case** (Weight: 7.5)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = True
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_1.svg)
---
<a name="00000-main-use-case-3"></a>
**Use case** (Weight: 8.4)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF5 = True
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_2.svg)
---
<a name="00000-main-use-case-4"></a>
**Use case** (Weight: 10.1)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF5 = False
WOW-MENU = True
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_3.svg)
---
<a name="00000-main-use-case-5"></a>
**Use case** (Weight: 11.7)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF5 = False
WOW-MENU = False
EIBAID EQUAL TO DFHENTER = True
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_4.svg)
---
<a name="00000-main-use-case-6"></a>
**Use case** (Weight: 9.7)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF5 = False
WOW-MENU = False
EIBAID EQUAL TO DFHENTER = False
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_5.svg)
---
<a name="00000-main-use-case-7"></a>
**Use case** (Weight: 6.4)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = True
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_6.svg)
---
<a name="00000-main-use-case-8"></a>
**Use case** (Weight: 6.2)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = True
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_7.svg)
---
<a name="00000-main-use-case-9"></a>
**Use case** (Weight: 7.1)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF5 = True
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_8.svg)
---
<a name="00000-main-use-case-10"></a>
**Use case** (Weight: 8.8)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF5 = False
WOW-MENU = True
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_9.svg)
---
<a name="00000-main-use-case-11"></a>
**Use case** (Weight: 10.4)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF5 = False
WOW-MENU = False
EIBAID EQUAL TO DFHENTER = True
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_10.svg)
---
<a name="00000-main-use-case-12"></a>
**Use case** (Weight: 8.4)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHPF5 = False
WOW-MENU = False
EIBAID EQUAL TO DFHENTER = False
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_00000-MAIN_11.svg)


<a name="doge-exit"></a>
## DOGE-EXIT
**Description**: Main entry point for DOGE-EXIT functionality

**Internal Calls**:
- DOGE-EXIT

**External Calls**:

**Archimate Diagram:**
![Diagram](svg/DOGECOIN_diagram_DOGE-EXIT.svg)
### Analyzed Paths
---
<a name="doge-exit-use-case-1"></a>
**Use case** (Weight: 1.5)

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_DOGE-EXIT_0.svg)


<a name="receive-option"></a>
## RECEIVE-OPTION
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
<a name="receive-option-use-case-1"></a>
**Use case** (Weight: 2.8)

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_RECEIVE-OPTION_0.svg)


<a name="parse-option"></a>
## PARSE-OPTION
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
<a name="parse-option-use-case-1"></a>
**Use case** (Weight: 4.5)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = True
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_PARSE-OPTION_0.svg)
---
<a name="parse-option-use-case-2"></a>
**Use case** (Weight: 5.9)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = True
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_PARSE-OPTION_1.svg)
---
<a name="parse-option-use-case-3"></a>
**Use case** (Weight: 7.3)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = False
OPTIONI EQUAL TO 'D' = True
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_PARSE-OPTION_2.svg)
---
<a name="parse-option-use-case-4"></a>
**Use case** (Weight: 8.7)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = False
OPTIONI EQUAL TO 'D' = False
OPTIONI EQUAL TO 'S' = True
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_PARSE-OPTION_3.svg)
---
<a name="parse-option-use-case-5"></a>
**Use case** (Weight: 5.9)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = False
OPTIONI EQUAL TO 'W' = False
OPTIONI EQUAL TO 'D' = False
OPTIONI EQUAL TO 'S' = False
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_PARSE-OPTION_4.svg)


<a name="doge-main-screen"></a>
## DOGE-MAIN-SCREEN
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
<a name="doge-main-screen-use-case-1"></a>
**Use case** (Weight: 18.8)

## Business Rules:
```
TDATE = 0000000002 = True
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_DOGE-MAIN-SCREEN_0.svg)
---
<a name="doge-main-screen-use-case-2"></a>
**Use case** (Weight: 18.5)

## Business Rules:
```
TDATE = 0000000002 = False
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_DOGE-MAIN-SCREEN_1.svg)


<a name="convert-amount-to-display"></a>
## CONVERT-AMOUNT-TO-DISPLAY
**Description**: Main entry point for CONVERT-AMOUNT-TO-DISPLAY functionality

**Internal Calls**:
- CONVERT-AMOUNT-TO-DISPLAY

**External Calls**:

**Archimate Diagram:**
![Diagram](svg/DOGECOIN_diagram_CONVERT-AMOUNT-TO-DISPLAY.svg)
### Analyzed Paths
---
<a name="convert-amount-to-display-use-case-1"></a>
**Use case** (Weight: 4.2)

## Business Rules:
```
TAMT-SIGN-NEGATIVE = True
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_CONVERT-AMOUNT-TO-DISPLAY_0.svg)
---
<a name="convert-amount-to-display-use-case-2"></a>
**Use case** (Weight: 2.9)

## Business Rules:
```
TAMT-SIGN-NEGATIVE = False
```

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_CONVERT-AMOUNT-TO-DISPLAY_1.svg)


<a name="convert-date"></a>
## CONVERT-DATE
**Description**: Main entry point for CONVERT-DATE functionality

**Internal Calls**:
- CONVERT-DATE

**External Calls**:
- FORMATTIME ABSTIME(TEMP-DATE) DATESEP('/') MMDDYYYY(DDATE)

**Archimate Diagram:**
![Diagram](svg/DOGECOIN_diagram_CONVERT-DATE.svg)
### Analyzed Paths
---
<a name="convert-date-use-case-1"></a>
**Use case** (Weight: 2.3)

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_CONVERT-DATE_0.svg)


<a name="doge-wto"></a>
## DOGE-WTO
**Description**: Main entry point for DOGE-WTO functionality

**Internal Calls**:
- DOGE-WTO

**External Calls**:
- WRITE OPERATOR TEXT(WTO-MESSAGE)

**Archimate Diagram:**
![Diagram](svg/DOGECOIN_diagram_DOGE-WTO.svg)
### Analyzed Paths
---
<a name="doge-wto-use-case-1"></a>
**Use case** (Weight: 2.3)

## Execution Path
![Diagram](svg/DOGECOIN_sl_diagram_DOGE-WTO_0.svg)

