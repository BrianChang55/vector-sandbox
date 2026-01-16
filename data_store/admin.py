from django.contrib import admin

from data_store.models import AppDataTable, AppDataRow, AppDataTableSnapshot


@admin.register(AppDataTable)
class AppDataTableAdmin(admin.ModelAdmin):
    list_display = ("name", "internal_app", "slug", "row_count", "created_at")
    search_fields = ("name", "internal_app__name")


@admin.register(AppDataRow)
class AppDataRowAdmin(admin.ModelAdmin):
    list_display = ("table", "row_index", "created_at")
    search_fields = ("table__name",)


@admin.register(AppDataTableSnapshot)
class AppDataTableSnapshotAdmin(admin.ModelAdmin):
    list_display = ("table_name", "app_version", "operation", "created_at")
    list_filter = ("operation",)
    search_fields = ("table_name",)
