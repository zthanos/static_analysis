Μπορείς να διατηρήσεις ένα «γλωσσικά-ουδέτερο» σύστημα κατηγοριών βασισμένο στον ρόλο/σκοπό κάθε statement, παρά στο όνομά του. Για παράδειγμα:

| Γενική Κατηγορία | Περιγραφή | Παράδειγμα COBOL Contexts |
| --- | --- | --- |
| **Assignment** | Ενημέρωση τιμής σε ένα πεδίο ή μεταβλητή | `MOVE`, `COMPUTE ... INTO`, `SET` |
| **Invocation** | Κλήση υπορουτίνας, συνάρτησης ή εξωτερικής υπηρεσίας | `CALL`, `EXEC CICS ...`, (και CICS commands ως RPC) |
| **Conditional** | Έλεγχος συνθήκης και διακλάδωση | `IF ... THEN`, `EVALUATE`, `SEARCH ... WHEN` |
| **Loop** | Επαναληπτικές δομές | `PERFORM ... UNTIL`, `SEARCH VARYING` |
| **Control-Transfer** | Άμεση αλλαγή ροής εκτέλεσης (εκτός από loop/cond) | `GO TO`, `EXIT`, `GOBACK`, `RETURN` |
| **I/O** | Είσοδος/έξοδος χρήστη ή αρχείου | `ACCEPT`, `DISPLAY`, `READ`, `WRITE`, `OPEN`, `CLOSE` |
| **Data-Operation** | Μετασχηματισμός ή ανάλυση δεδομένων | `SORT`, `INSPECT`, `INITIALIZE` |
| **Resource-Management** | Διαχείριση πόρων ή περιβάλλοντος | `OPEN`, `CLOSE`, `INITIALIZE`, `CANCEL`, `RELEASE` |
| **Transaction/Service** | Εντολές transactional ή εξυπηρέτησης | `EXEC CICS ...`, `COMMITMENT CONTROL`, `START` |
| **Other** | Οτιδήποτε δεν ταιριάζει στις παραπάνω | --- |

### Πώς το εφαρμόζεις:

1.  **Χαρτογράφηση:** Κάνε έναν πίνακα pairing από κάθε `*StatementContext` στο κατάλληλο enum.

    -   `AcceptStatementContext` → **I/O**

    -   `SortStatementContext` → **Data-Operation**

    -   `GotoStatementContext` → **Control-Transfer**

    -   `CancelStatementContext` → **Resource-Management**

    -   `ComputeStatementContext` → **Assignment** (ή Data-Operation αν το θεωρείς μετασχηματισμό)

    -   κ.ο.κ.

2.  **Επεκτασιμότητα:** Όταν προστεθεί νέο context ή σε άλλη γλώσσα, απλώς κάνεις map στο κοντινότερο generic role, χωρίς να αλλάζεις enums.

3.  **Στο μοντέλο:** Εκπαιδεύεις το classification task ώστε, για κάθε κόμβο AST, να προβλέπει το role (π.χ. "I/O" vs. "Loop"), πράγμα που λειτουργεί σε COBOL, Java, Python κ.λπ.

Με αυτό το σχήμα κρατάς λίγες, στοχευμένες κατηγορίες και μέγιστη γενίκευση.