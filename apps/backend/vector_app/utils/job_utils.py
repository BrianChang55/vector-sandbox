"""Job-related utility functions."""
import time


def append_job_event(job, event_type: str, data: dict):
    """
    Append an event to the job's events_json.

    Uses atomic DB update to ensure events are stored reliably.
    Can be used from both tasks.py and views.
    """
    event = {
        'type': event_type,
        'data': data,
        'timestamp': time.time(),
        'index': len(job.events_json),
    }
    job.events_json.append(event)
    job.chunk_count = len(job.events_json)
    job.save(update_fields=['events_json', 'chunk_count', 'updated_at'])
