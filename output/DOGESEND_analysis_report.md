# DOGESEND
**File**: DOGESEND.cbl

**Program**: DOGESEND

**Language**: COBOL

---
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
**Use case** (Weight: 6.9)

## Business Rules:
```
EIBCALEN > ZERO = True
EIBCALEN EQUAL TO ZERO = True
```

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_DOGE-MAIN_0.svg)
---
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
**Use case** (Weight: 5.6)

## Business Rules:
```
EIBCALEN > ZERO = False
EIBCALEN EQUAL TO ZERO = True
```

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_DOGE-MAIN_4.svg)
---
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
**Use case** (Weight: 2.3)

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_DOGE-WTO_0.svg)


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
**Use case** (Weight: 2.0)

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_RECEIVE-INPUT_0.svg)


## PARSE-INPUT
**Description**: Main entry point for PARSE-INPUT functionality

**Internal Calls**:
- PARSE-INPUT

**External Calls**:

**Archimate Diagram:**
![Diagram](svg/DOGESEND_diagram_PARSE-INPUT.svg)
### Analyzed Paths
---
**Use case** (Weight: 1.4)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = True
```

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_PARSE-INPUT_0.svg)
---
**Use case** (Weight: 1.4)

## Business Rules:
```
OPTIONI EQUAL TO 'T' = False
```

## Execution Path
![Diagram](svg/DOGESEND_sl_diagram_PARSE-INPUT_1.svg)

