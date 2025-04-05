# DOGESEND
**File**: DOGESEND.cbl

**Program**: DOGESEND

**Language**: COBOL

---
# Table of Contents
- [DOGE-MAIN](#doge-main)
  - [Use case 1 (Weight: 6.9)](#doge-main-use-case-1)
  - [Use case 2 (Weight: 7.5)](#doge-main-use-case-2)
  - [Use case 3 (Weight: 8.9)](#doge-main-use-case-3)
  - [Use case 4 (Weight: 6.9)](#doge-main-use-case-4)
  - [Use case 5 (Weight: 5.6)](#doge-main-use-case-5)
  - [Use case 6 (Weight: 6.2)](#doge-main-use-case-6)
  - [Use case 7 (Weight: 7.6)](#doge-main-use-case-7)
  - [Use case 8 (Weight: 5.6)](#doge-main-use-case-8)
- [DOGE-WTO](#doge-wto)
  - [Use case 1 (Weight: 2.3)](#doge-wto-use-case-1)
- [RECEIVE-INPUT](#receive-input)
  - [Use case 1 (Weight: 2.0)](#receive-input-use-case-1)
- [PARSE-INPUT](#parse-input)
  - [Use case 1 (Weight: 1.4)](#parse-input-use-case-1)
  - [Use case 2 (Weight: 1.4)](#parse-input-use-case-2)
---
<a name="doge-main"></a>
## DOGE-MAIN
**Description**: Main entry point for DOGE-MAIN functionality

**Internal Calls**:
- DOGE-MAIN
- DOGE-WTO
- PARSE-INPUT
- RECEIVE-INPUT

**External Calls**:
- RETURN TRANSID('DSND') COMMAREA(DOGECOMMS-AREA)
- SEND MAP('DOGESN1') MAPSET('DOGESN') ERASE
- XCTL PROGRAM('DOGEQUIT')

**Archimate Diagram:**
![Diagram](svg/DOGESEND_diagram_DOGE-MAIN.svg)
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
![Diagram](svg/DOGESEND_sl_diagram_DOGE-MAIN_0.svg)
---
<a name="doge-main-use-case-2"></a>
**Use case** (Weight: 7.5)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = True
```

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_DOGE-MAIN_1.svg)
---
<a name="doge-main-use-case-3"></a>
**Use case** (Weight: 8.9)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHENTER = True
```

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_DOGE-MAIN_2.svg)
---
<a name="doge-main-use-case-4"></a>
**Use case** (Weight: 6.9)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHENTER = False
```

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_DOGE-MAIN_3.svg)
---
<a name="doge-main-use-case-5"></a>
**Use case** (Weight: 5.6)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = True
```

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_DOGE-MAIN_4.svg)
---
<a name="doge-main-use-case-6"></a>
**Use case** (Weight: 6.2)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = True
```

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_DOGE-MAIN_5.svg)
---
<a name="doge-main-use-case-7"></a>
**Use case** (Weight: 7.6)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHENTER = True
```

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_DOGE-MAIN_6.svg)
---
<a name="doge-main-use-case-8"></a>
**Use case** (Weight: 5.6)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = False
EIBAID EQUAL TO DFHPF3 = False
EIBAID EQUAL TO DFHENTER = False
```

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_DOGE-MAIN_7.svg)


<a name="doge-wto"></a>
## DOGE-WTO
**Description**: Main entry point for DOGE-WTO functionality

**Internal Calls**:
- DOGE-WTO

**External Calls**:
- WRITE OPERATOR TEXT(WTO-MESSAGE)

**Archimate Diagram:**
![Diagram](svg/DOGESEND_diagram_DOGE-WTO.svg)
### Analyzed Paths
---
<a name="doge-wto-use-case-1"></a>
**Use case** (Weight: 2.3)

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_DOGE-WTO_0.svg)


<a name="receive-input"></a>
## RECEIVE-INPUT
**Description**: Main entry point for RECEIVE-INPUT functionality

**Internal Calls**:
- RECEIVE-INPUT

**External Calls**:
- RECEIVE MAP('DOGESN1') MAPSET('DOGESN') INTO(DOGESN1I) ASIS

**Archimate Diagram:**
![Diagram](svg/DOGESEND_diagram_RECEIVE-INPUT.svg)
### Analyzed Paths
---
<a name="receive-input-use-case-1"></a>
**Use case** (Weight: 2.0)

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_RECEIVE-INPUT_0.svg)


<a name="parse-input"></a>
## PARSE-INPUT
**Description**: Main entry point for PARSE-INPUT functionality

**Internal Calls**:
- PARSE-INPUT

**External Calls**:

**Archimate Diagram:**
![Diagram](svg/DOGESEND_diagram_PARSE-INPUT.svg)
### Analyzed Paths
---
<a name="parse-input-use-case-1"></a>
**Use case** (Weight: 1.4)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = True
```

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_PARSE-INPUT_0.svg)
---
<a name="parse-input-use-case-2"></a>
**Use case** (Weight: 1.4)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = False
```

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_PARSE-INPUT_1.svg)

