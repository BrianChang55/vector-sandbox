from pydantic import BaseModel, ConfigDict


class StrictBaseModel(BaseModel):
    """
    Base Pydantic model with strict validation.

    All models inheriting from this class will:
    - Reject any extra fields not defined in the model (extra="forbid")
    - Not coerce types (strict=True) - e.g., "1" will not be accepted for int fields

    Usage:
        from vector_app.utils.pydantic_utils import StrictBaseModel

        class MyModel(StrictBaseModel):
            count: int
            name: str

        # This will raise ValidationError (extra field)
        MyModel(count=1, name="test", extra_field="bad")

        # This will raise ValidationError (string "1" not coerced to int)
        MyModel(count="1", name="test")

        # This works
        MyModel(count=1, name="test")
    """

    model_config = ConfigDict(
        extra="forbid",
        strict=True,
    )

