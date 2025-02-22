from typing import List, Optional
from src.static_analysis.logger import logger

def get_children(ctx) -> List:
    """
    Επιστρέφει μια λίστα με όλα τα παιδικά nodes ενός context.

    :param ctx: Το αντικείμενο context.
    :return: Λίστα από child nodes.
    """
    return list(ctx.getChildren())

def print_child(ctx) -> None:
    """
    Εκτυπώνει το text του δεδομένου context.

    :param ctx: Το αντικείμενο context.
    """
    logger.info(ctx.getText())

def get_child_text(ctx, idx: int) -> Optional[str]:
    """
    Επιστρέφει το text του child node στη συγκεκριμένη θέση.

    :param ctx: Το αντικείμενο context.
    :param idx: Η θέση του child node.
    :return: Το text του child node ή None αν η θέση είναι εκτός ορίων.
    """
    try:
        return ctx.getChild(idx).getText()
    except (IndexError, AttributeError):
        return None

def get_child_concatenated_text(ctx, idx: int) -> str:
    """
    Επιστρέφει το concatenated text όλων των child nodes του συγκεκριμένου child node.

    :param ctx: Το αντικείμενο context.
    :param idx: Η θέση του child node.
    :return: Concatenated text όλων των παιδιών, χωρισμένο με space.
    """
    try:
        ch = ctx.getChild(idx)
        if ch.getChildCount() == 0:  # Αν δεν έχει παιδιά, επιστρέφουμε το δικό του text
            return ch.getText()
        return " ".join(child.getText() for child in ch.getChildren())
    except (IndexError, AttributeError):
        return ""

def print_class_name(obj):
    """
    Debugging utility: Εκτυπώνει το όνομα της κλάσης ενός αντικειμένου.

    :param obj: Το αντικείμενο που εξετάζεται.
    """
    logger.info(obj.__class__.__name__)
