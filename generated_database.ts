/**
 * Database Type Definitions
 *
 * AUTO-GENERATED - DO NOT EDIT MANUALLY
 *
 * This file contains TypeScript type definitions for all database tables.
 * Import these types to get compile-time type safety for dataStore operations.
 *
 * @example
 * ```typescript
 * import { Task, Database } from './types/database';
 * import { dataStore } from './lib/dataStore';
 *
 * // Type-safe insert
 * const newTask: Database['tasks']['insert'] = {
 *   title: 'New task',
 *   status: 'pending'
 * };
 * await dataStore.insert('tasks', newTask);
 *
 * // Type-safe query
 * const result = await dataStore.query('tasks', {});
 * const tasks: Task[] = result.rows;
 * ```
 */

/**
 * Tasks
 * Task management for projects
 * 
 * Table: `tasks` (42 rows)
 */
export interface Tasks {
  /** Primary key, Auto-generated */
  id?: string;
  title: string;
  description?: string | null;
  status?: 'pending' | 'in_progress' | 'done' | 'blocked';
  priority?: number | null;
  completed?: boolean | null;
  /** Auto-set on create */
  created_at?: string;
  /** Auto-set on update */
  updated_at?: string;
}

/**
 * Insert type for Tasks
 * Excludes auto-generated fields
 */
export interface TasksInsert {
  title: string;
  description?: string | null;
  status?: 'pending' | 'in_progress' | 'done' | 'blocked';
  priority?: number | null;
  completed?: boolean | null;
}

/**
 * Update type for Tasks
 * All fields optional, excludes primary key and auto-fields
 */
export interface TasksUpdate {
  title?: string;
  description?: string | null;
  status?: 'pending' | 'in_progress' | 'done' | 'blocked';
  priority?: number | null;
  completed?: boolean | null;
  created_at?: string;
}

/**
 * Database schema mapping
 * Maps table slugs to their types
 */
export interface Database {
  'tasks': {
    row: Tasks;
    insert: TasksInsert;
    update: TasksUpdate;
  };
}

/**
 * Valid table slugs
 * Use this type for type-safe table slug references
 */
export type TableSlug = 'tasks';