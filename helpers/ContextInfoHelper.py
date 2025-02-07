from typing import List, Optional
from logger import logger

class ContextInfoHelper:
    """
    Helper class for working with context objects.
    Provides utility methods to extract and manipulate child nodes.
    """

    @staticmethod
    def get_children(ctx) -> List:
        """
        Returns a list of all child nodes of the given context.

        :param ctx: The context object.
        :return: List of child nodes.
        """
        return list(ctx.getChildren())

    @staticmethod
    def print_child(ctx) -> None:
        """
        Prints the text of the given context.

        :param ctx: The context object.
        """
        logger.info(ctx.getText())

    @staticmethod
    def get_child_text(ctx, idx: int) -> Optional[str]:
        """
        Returns the text of the child node at the specified index.

        :param ctx: The context object.
        :param idx: Index of the child node.
        :return: Text of the child node, or None if the index is out of bounds.
        """
        try:
            return ctx.getChild(idx).getText()
        except (IndexError, AttributeError):
            return None

    @staticmethod
    def get_child_concatenated_text(ctx, idx: int) -> str:
        """
        Returns the concatenated text of all child nodes of the specified child node.

        :param ctx: The context object.
        :param idx: Index of the child node whose children will be concatenated.
        :return: Concatenated text of all child nodes, separated by spaces.
        """
        try:
            ch = ctx.getChild(idx)
            if ch.getChildCount() == 0:  # Αν δεν έχει παιδιά, επιστρέφουμε το δικό του text
                return ch.getText()
            return " ".join(child.getText() for child in ch.getChildren())
        except (IndexError, AttributeError):
            return ""

    @staticmethod
    def print_class_name(obj):
        """
        Debugging utility: Prints the class name of an object.

        :param obj: The object whose class name is to be printed.
        """
        logger.info(obj.__class__.__name__)
