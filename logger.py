import logging

# Δημιουργία του logger
logger = logging.getLogger("StaticAnalysisLogger")
logger.setLevel(logging.DEBUG) 

# Δημιουργία Handler (καταγραφή σε αρχείο και κονσόλα)
console_handler = logging.StreamHandler()
file_handler = logging.FileHandler("static_analysis.log")

# Ρύθμιση του format για την καταγραφή
formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')

console_handler.setFormatter(formatter)
file_handler.setFormatter(formatter)

# Προσθήκη handlers στον logger
#logger.addHandler(console_handler)
logger.addHandler(file_handler)
