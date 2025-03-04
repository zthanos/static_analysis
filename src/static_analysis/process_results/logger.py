import logging

def setup_logger(name, log_file, level=logging.DEBUG):
    """Function to setup a logger"""
    formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
    console_handler = logging.StreamHandler()
    handler = logging.FileHandler(log_file)        
    console_handler.setFormatter(formatter)
    handler.setFormatter(formatter)

    logger = logging.getLogger(name)
    logger.setLevel(level)
    logger.addHandler(handler)
    logger.addHandler(console_handler)
    
    return logger

logger = setup_logger('AnalyzerLogger', 'analyzer.log')

