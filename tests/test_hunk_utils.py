from vector_app.services.diff import FileDiff, _reconstruct_diff, apply_diff

def test_duplicate_hunks_last_wins():
    """Test that duplicate hunks (same start line) use the last one."""
    original = "line 1\nline 2\nline 3\nline 4\nline 5\nline 55\nline 7\nline 8"
    
    # Two hunks targeting line 55 (index 6 in 1-based, but let's say it's at line 6)
    hunk1 = """@@ -6,1 +6,1 @@
-line 55
+line 55 v1"""
    
    hunk2 = """@@ -6,1 +6,1 @@
-line 55
+line 55 v2"""
    
    diff = FileDiff(path="test.txt", hunks=[hunk1, hunk2])
    
    # Reconstruct should deduplicate and keep hunk2
    reconstructed = _reconstruct_diff(diff)
    assert "line 55 v2" in reconstructed
    assert "line 55 v1" not in reconstructed
    
    # Apply should work
    result = apply_diff(original, diff)
    assert "line 55 v2" in result
    assert "line 55 v1" not in result

def test_misordered_hunks_reordered():
    """Test that misordered hunks are reordered correctly."""
    original = "line 10\nline 20\nline 30\nline 40"
    
    hunk_late = """@@ -3,1 +3,1 @@
-line 30
+line 30 modified"""
    
    hunk_early = """@@ -1,1 +1,1 @@
-line 10
+line 10 modified"""
    
    # Provided in wrong order: late then early
    diff = FileDiff(path="test.txt", hunks=[hunk_late, hunk_early])
    
    reconstructed = _reconstruct_diff(diff)
    
    # Check that hunk_early comes before hunk_late in reconstructed diff
    early_idx = reconstructed.find("@@ -1,1")
    late_idx = reconstructed.find("@@ -3,1")
    
    assert early_idx != -1
    assert late_idx != -1
    assert early_idx < late_idx
    
    # Apply should work
    result = apply_diff(original, diff)
    assert "line 10 modified" in result
    assert "line 30 modified" in result

def test_reordering_and_deduplication_tandem():
    """Test that both reordering and deduplication work together with multiple duplicates."""
    original = "line 1\nline 2\nline 3\nline 4\nline 5\nline 6"
    
    # Hunks in random order with duplicates
    # Line 5 (index 5) has 3 versions
    h5_v1 = "@@ -5,1 +5,1 @@\n-line 5\n+line 5 v1"
    h5_v2 = "@@ -5,1 +5,1 @@\n-line 5\n+line 5 v2"
    h5_v3 = "@@ -5,1 +5,1 @@\n-line 5\n+line 5 v3"
    
    # Line 2 (index 2) has 1 version
    h2 = "@@ -2,1 +2,1 @@\n-line 2\n+line 2 modified"
    
    # Line 4 (index 4) has 1 version
    h4 = "@@ -4,1 +4,1 @@\n-line 4\n+line 4 modified"
    
    # Mixed order: [h5_v1, h2, h5_v2, h4, h5_v3]
    hunks = [h5_v1, h2, h5_v2, h4, h5_v3]
    diff = FileDiff(path="test.txt", hunks=hunks)
    
    reconstructed = _reconstruct_diff(diff)
    
    # Should be sorted: line 2, then line 4, then line 5 (v3)
    idx2 = reconstructed.find("@@ -2,1")
    idx4 = reconstructed.find("@@ -4,1")
    idx5 = reconstructed.find("@@ -5,1")
    
    assert idx2 < idx4 < idx5
    assert "line 5 v3" in reconstructed
    assert "line 5 v1" not in reconstructed
    assert "line 5 v2" not in reconstructed
    
    # Apply
    result = apply_diff(original, diff)
    expected = "line 1\nline 2 modified\nline 3\nline 4 modified\nline 5 v3\nline 6"
    assert result == expected
