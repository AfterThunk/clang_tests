#! /usr/bin/python3

import json
import sys
import os
import argparse
import re
import shlex
from functools import partial

import clang
import clang.cindex


def __get_full_file_name(name):
	return os.path.abspath(os.path.realpath(os.path.expanduser(name)))


class CompileCommands():
	'''
		Wrapper around CompileCommands that lazy evaluates and caches
		the results for use later.
	'''

	def __init__(self, compile_commands_path):
		self.path = compile_commands_path
		self.dir = os.path.dirname(self.path)
		self._commands = None
		self._command_path_to_index = None

	def get_command(self, file_to_compile):
		if not self._commands:
			# Open the specified file. This should contain an array of JSON
			# objects representing compile commands.
			with open(self.path, 'r') as compile_commands_file:
				self._commands = json.loads(compile_commands_file.read())

			# Now populate our lookup table
			self._command_path_to_index = {
				self._commands[i]['file'] : i
				for i in range(len(self._commands))
			}

		return self._commands[self._command_path_to_index[file_to_compile]]

	def get_command_directives(self, command):
		if isinstance(command, dict):
			command = command['command']

		# Ignore the first and last items, as those should just be the
		# compiler and file name respectively.
		directives = shlex.split(command)[1:-1]

		# Now go through and expand any @ directives.
		# These indicate that we should substitute a files text
		# (specified by the directive) with the directive itself.
		replacements = []
		for i in range(len(directives)):
			if directives[i][0] == '@':
				file_name = directives[i][1:]
				file_name = os.path.join(self.dir, file_name)
				with open(file_name, 'r') as expanded_contents:
					replacement = shlex.split(expanded_contents.read())
					replacements += [(i, replacement)]

		# Swap the old directives with the expanded directives
		for i, repl in replacements:
			directives[i:i+1] = repl

		return directives


class WrappedCursor():
	'''
		Wraps a clang cursor and adds a Parent.
		This can typically used when iterating or visiting cursors,
		but parent may be invalid in cases when it's unknown (e.g.
		going to a definition / declaration).
	'''


	def __init__(self, cursor, parent):

		unwound_cursor = cursor
		while isinstance(unwound_cursor, WrappedCursor):
			unwound_cursor = unwound_cursor._cursor

		self._cursor = unwound_cursor
		self._parent = parent

	@property
	def parent(self):
		return self._parent

	def __getattr__(self, attr):
		if attr == 'parent':
			return self._parent

		return getattr(self._cursor, attr)

	def __hash__(self):
		return hash(self._cursor)

	def __eq__(self, other):
		if isinstance(other, WrappedCursor):
			return self._cursor == other._cursor
		else:
			return self._cursor == other


def load_ue4_translation_unit(file_path, ast_cache_path, compile_commands, parse_options=0):

	translation_unit = None
	if ast_cache_path:
		try:
			translation_unit = clang.cindex.TranslationUnit.from_ast_file(ast_cache_path)
		except clang.cindex.TranslationUnitLoadError:
			print('Failed to load cached AST')

	if translation_unit is None:
		command = compile_commands.get_command(file_path)
		directives = compile_commands.get_command_directives(command)

		index = clang.cindex.Index.create()
		translation_unit = index.parse(file_path, args=directives, options=parse_options)

		if ast_cache_path:
			try:
				translation_unit.save(ast_cache_path)
			except clang.cindex.TranslationUnitSaveError:
				print('Failed to cache AST')

	return translation_unit


def foreach_cursor(action, cursor, depth=0):
	'''
		Recursively iterates over a cursors children, applying
		a specified action.
		This applies the action to the given cursor as well.

		The action should be some callable that takes a WrappedCursor and int depth.
	'''
	action(cursor, depth)
	for child in cursor.get_children():
		foreach_cursor(action, WrappedCursor(child, cursor), depth+1)


def find_cursor(predicate, cursor):
	'''
		Searches the given cursor's direct children, looking for one
		that satisfies the given predicate.
		Only returns the first matching cursor.

		Predicates should be some callable that takes a single argument, a  WrappedCursor.

		@return A matching cursor if found, None otherwise.
	'''
	for child in cursor.get_children():
		wrapped_child = WrappedCursor(child, cursor)
		if predicate(wrapped_child):
			return wrapped_child


def find_cursor_recursive(predicate, cursor):
	'''
		Searches the given cursor's children recursively, looking for one
		that satisfies the given predicate.
		Only returns the first matching cursor.

		Predicates should be some callable that takes a single argument, a  WrappedCursor.

		@return A matching cursor if found, None otherwise.
	'''
	for child in cursor.get_children():
		wrapped_child = WrappedCursor(child, cursor)
		if predicate(wrapped_child):
			return wrapped_child

		found_in_child = find_cursor_recursive(predicate, wrapped_child)
		if found_in_child:
			return found_in_child


def find_cursor_many(predicate, cursor):
	'''
		Searches the given cursor's direct children, looking for any
		that satisfies the given predicate.

		Predicates should be some callable that takes a single argument, a  WrappedCursor.

		@return A list of matching cursors, or empty list if none were found.
	'''
	found = []
	for child in cursor.get_children():
		wrapped_child = WrappedCursor(child, cursor)
		if predicate(wrapped_child):
			found += [wrapped_child]

	return found


def find_cursor_recursive_many(predicate, cursor):
	'''
		Searches the given cursor's children recursively, looking for any
		that satisfy the given predicate.
		Note, this **will not** recurse into children of found matches.

		Predicates should be some callable that takes a single argument, a  WrappedCursor.

		@return A list of matching cursors, or empty list if none were found.

	'''
	found = []
	for child in cursor.get_children():
		wrapped_child = WrappedCursor(child, cursor)
		if predicate(wrapped_child):
			found += [wrapped_child]

		else:
			found += find_cursor_recursive_many(predicate, wrapped_child)

	return found


def find_cursor_recursive_all(predicate, cursor):
	'''
		Searches the given cursor's children recursively, looking for any
		that satisfy the given predicate.
		Note, this **will** recurse into the children of found matches.

		Predicates should be some callable that takes a single argument, a  WrappedCursor.

		@return A list of matching cursors, or empty list if none were found.
	'''
	found = []
	for child in cursor.get_children():
		wrapped_child(child, cursor)
		if predicate(wrapped_child):
			found += [wrapped_child]

		found += find_cursor_recursive_all(predicate, wrapped_child)

	return found


def foreach_parent(action, cursor, depth=0):
	'''
		Recursively iterates over a cursor'ss parents, applying
		a specified action.
		This applies the action to the given cursor as well.

		The action should be some callable that takes a WrappedCursor and int depth.
	'''
	if cursor is not None:
		action(cursor, depth)

		if hasattr(cursor, 'parent'):
			foreach_parent(action, cursor.parent, depth+1)


def find_parent(predicate, cursor):
	'''
		Searches the given cursor's parents recursively, looking for any
		that satisfy the given predicate.
		Note, this **will not** recurse into the parents of found matches.

		Predicates should be some callable that takes a single argument, a  WrappedCursor.

		@return A matching cursor if found, None otherwise.
	'''

	if cursor.parent is not None:
		if predicate(cursor.parent):
			return cursor.parent
		else:
			return find_parent(predicate, cursor.parent)


def find_parent_many(predicate, cursor):
	'''
		Searches the given cursor's parents recursively, looking for any
		that satisfy the given predicate.
		Note, this **will** recurse into the parents of found matches.

		Predicates should be some callable that takes a single argument, a  WrappedCursor.

		@return A list of matching cursors, or empty list if none were found.
	'''

	found = []
	if cursor.parent is not None:
		if predicate(cursor.parent):
			found += [cursor.parent]

		found += find_parent_many(predicate, cursor.parent)

	return found


def freeze_predicates(predicates):
	'''
		Convenience method that freezes a set of predicates.

		Input is expected to be an iterable of lists.
		Each list is treated as a single predicate to be frozen.
		The first element in each list should be a callable.
		All remaining elements in the list are non-keyword arguments
		to be applied to the callable in order.
	'''

	return [partial(predicate[0], *predicate[1:]) for predicate in predicates]


def cursor_matches_all(*predicates):
	'''
		Returns a predicate that is true only if all given
		predicates are true.

		@see freeze_predicates for an explanation of inputs.
		It's expected that each given predicate also accepts
		a cursor as its last non-keyword argument.

		Note, this may end up evaluating all predicates, even if
		some are found to be false.
	'''

	predicates = freeze_predicates(predicates)
	def matches_all(cursor):
		return all((predicate(cursor) for predicate in predicates))

	return matches_all


def cursor_matches_any(*predicates):
	'''
		Returns a predicate that is true if any of the given
		predicates are true.

		@see freeze_predicates for an explanation of inputs.
		It's expected that each given predicate also accepts
		a cursor as its last non-keyword argument.

		Note, this may end up evaluating all predicates, even if
		some are found to be true.
	'''

	predicates = freeze_predicates(predicates)
	def matches_any(cursor):
		return any((predicate(cursor) for predicate in predicates))

	return matches_any


if __name__ == '__main__':

	argument_parser = argparse.ArgumentParser('GetCompileCommands')
	argument_parser.add_argument('file_name', help='Path to file for which commands should be received.')
	argument_parser.add_argument('-c', '--compile_commands', help='Path to compile_commands.json', default='compile_commands.json')
	argument_parser.add_argument('-l', '--llvm_libs', help='Directory that contains LLVM libraries', default='/usr/lib/llvm-7/lib')
	argument_parser.add_argument('-f', '--cache_file', help='Points to a saved AST file. If no file is found, or the file is invalid, a the source will be parsed and saved here for future iterations.', default=None)
	arguments = argument_parser.parse_args()

	compile_commands = CompileCommands(__get_full_file_name(arguments.compile_commands))
	file_path = __get_full_file_name(arguments.file_name)
	llvm_lib_dir = __get_full_file_name(arguments.llvm_libs)
	cached_ast_path = __get_full_file_name(arguments.cache_file) if arguments.cache_file else None

	# Make sure we set the library path before doing anything else.
	clang.cindex.Config.set_library_path(llvm_lib_dir)
	translation_unit = load_ue4_translation_unit(file_path, cached_ast_path, compile_commands)


	'''
		We want to find anywhere in NetworkProfiler.cpp that is serializing
		data to FileWriter.

		We'll take blocks of related serializations, and form those into types.

		This is how that's approached:

		Searching and Aggregation:

			1. Find the FNetworkProfiler class.
			2. Find the ENetworkProfilingPayloadType enum.
			3. Find the declaration of FileWriter.
			4. Find all methods inside FNetworkProfiler.
			5. Find all calls within each method.
			6. Remove any calls that don't reference FileWriter,
				or that seem to be inappropriate (e.g., checks).

		Grouping:

			Iterate over each method.
			Iterate over each call.

			1. Inspect the call's arguments.
			2. If any arguments are references to ENetworkProfilingPayload values,
				then we start a new group.
				This call is the first call in the new group.
			3. If no arguments reference ENetworkProfilingPayload, and there's an existing group, append this call to that group.
			4. If no arguments reference ENetworkProfilingPayload, and there's no existing group, disregard the call.

		Generation:

			Each ENetworkProfilingPayload referenced will dictate a new type.
			Each type will have one member variable per argument for every other call in its group.
			Each member variable will be named corresponding to the variable used in its call.
			Each member variable will be private, with corresponding const getter methods.
			Each type will have a Serialize method that copies the original set of calls (minus the Type serialization).
			Each type will have a constructor, accepting a value for each member variable, and an initializer list initializing all variables.

		The following is actual output from the command.

			USTRUCT()
			struct FNetworkProfilerType_SendBunch : public FNetworkProfilerType_Base
			{
			public:
				GENERATED_BODY();
				NETWORK_PROFILER_BODY(SendBunch);

				FNetworkProfilerType_SendBunch(const uint16 InChannelIndex, const uint16 InNumBits, const uint32 InNameTableIndex)
					: ChannelIndex(InChannelIndex)
					, NumBits(InNumBits)
					, NameTableIndex(InNameTableIndex)
				{
				}

				~FNetworkProfilerType_SendBunch()
				{
				}

				const uint16 GetChannelIndex() const
				{
					return ChannelIndex;
				}

				const uint16 GetNumBits() const
				{
					return NumBits;
				}

				const uint32 GetNameTableIndex() const
				{
					return NameTableIndex;
				}

				void Serialize(FArchive& Ar)
				{
					Ar << ChannelIndex;
					Ar.SerializeIntPacked(NameTableIndex);
					Ar << NumBits;
				}

			private:
				uint16 ChannelIndex;
				uint16 NumBits;
				uint32 NameTableIndex;
			};
			NETWORK_PROFILER_TYPE_TRAITS(SendBunch);

			FNetworkProfilerType_SendBunch SendBunch();
			SendBunch.Serialize(*FileWriter);
			<SourceLocation file '/home/widmo/Projects/UnrealEngine/Engine/Source/Runtime/Engine/Private/NetworkProfiler.cpp', line 416, column 3>


		These serialization bits were found around line 416:

			(*FileWriter) << Type;
			(*FileWriter) << ChannelIndex;
			(*FileWriter).SerializeIntPacked(NameTableIndex);
			(*FileWriter) << NumBits;

		From the generation, we can tell:

			Type was resolved to be NPTYPE_SendBunch.
			ChannelIndex was resolved to be a uint16.
			NameTableIndex was resolved to be uint32.
			NumBits was resolved to be uint16.

			Variables were sorted on size before being declared.
			Serialization snippets were pulled from the source code (order unchanged).
	'''

	# Get our translation unit.
	translation_unit_cursor = WrappedCursor(translation_unit.cursor, None)


	# Define a few repeated convenience methods
	# These will be composed using the utility methods above.
	def cursor_is_of_kind(kind, cursor):
		return cursor.kind == kind

	def cursor_is_of_type(type, cursor):
		return type == cursor.type

	def cursor_is_named(name, cursor):
		return cursor.spelling == name

	def cursor_name_starts_with(name, cursor):
		return cursor.spelling.startswith(name)

	def cursor_references(referenced, cursor):
		return referenced == cursor.referenced


	# Find FNetworkProfiler.
	# We do this by searching all top level children of the Translation Unit
	# for a cursor with kind CLASS_DECL and name 'FNetworkProfiler'
	network_profiler_class = find_cursor(
		cursor_matches_all(
			[cursor_is_of_kind, clang.cindex.CursorKind.CLASS_DECL],
			[cursor_is_named, 'FNetworkProfiler']
		),
		translation_unit_cursor
	)

	# Find ENetworkProfilingPayloadType
	# We do this by searching all top level children of the Translation Unit
	# for a cursor with kind ENUM_DECL and name 'ENetworkProfilingPayloadType'
	network_profiling_payload_enum = find_cursor(
		cursor_matches_all(
			[cursor_is_of_kind, clang.cindex.CursorKind.ENUM_DECL],
			[cursor_is_named, 'ENetworkProfilingPayloadType']
		),
		translation_unit_cursor
	)

	# Find all FNetworkProfiler Method Declarations.
	method_decls = find_cursor_many(
		cursor_matches_all(
			[cursor_is_of_kind, clang.cindex.CursorKind.CXX_METHOD]
		),
		network_profiler_class
	)

	# Get the corresponding Method Definitions.
	method_defs = (
		WrappedCursor(method_decl.get_definition(), translation_unit_cursor)
		for method_decl in method_decls
	)


	# Find the FNetworkProfiler::FileWriter.
	file_writer_decl = find_cursor(
		cursor_matches_all(
			[cursor_is_of_kind, clang.cindex.CursorKind.FIELD_DECL],
			[cursor_is_named, 'FileWriter']
		),
		network_profiler_class
	)

	# Find all calls referencing FNetworkProfiler::FileWriter.
	# We do this by recursively searching all methods of FNetworkProfiler
	# for cursor of type CALL_EXPR, and we make sure the name is either
	# operator<< or starts with Serialize. (TODO: there's probably a better
	# way to filter those...)
	#
	# Once we find such a cursor, we recursively search it for a reference
	# to FileWriter.
	calls_referencing_file_writer = (
		(method_def,
		find_cursor_recursive_many(
			cursor_matches_all(
				[cursor_is_of_kind, clang.cindex.CursorKind.CALL_EXPR],
				[
					cursor_matches_any(
						[cursor_is_named, 'operator<<'],
						[cursor_name_starts_with, 'Serialize']
					)
				],
				[
					find_cursor_recursive,
					cursor_matches_all(
						[cursor_is_of_kind, clang.cindex.CursorKind.MEMBER_REF_EXPR],
						[cursor_references, file_writer_decl]
					)
				]
			),
			method_def
		))
		for method_def in method_defs
	)

	# Cull out any non-matched methods.
	calls_referencing_file_writer = [
		(method_def, calls)
		for method_def, calls in calls_referencing_file_writer
		if len(calls) > 0
	]


	# On to the grouping phase!

	# Finding a reference back to ENetworkProfilingPayloadType is a bit trickier.
	# We need to recurse into the call, find any cursors referencing variables,
	# go to those variable declarations, and recurse into them.


	groups = []
	outstanding_group = None
	for method, calls in calls_referencing_file_writer:

		# Make sure we track any oustanding groups.
		if outstanding_group:
			groups += [outstanding_group]
			outstanding_group = None


		#print(method.spelling)
		for call in calls:


			# Find all the variables this call is using,
			# making sure to cull out things we don't want (like function prototypes).
			variable_refs = [
				variable_ref
				for variable_ref in find_cursor_recursive_many(
					cursor_matches_all(
						[cursor_is_of_kind, clang.cindex.CursorKind.DECL_REF_EXPR],
						[lambda cursor: cursor.type.kind is not clang.cindex.TypeKind.FUNCTIONPROTO]
					),
					call
				)
			]

			#print(call.spelling)

			for variable_ref in variable_refs:

				# See if the variable refers to an ENetworkProfilingPayloadType.
				# We recurse into the cursor that is referenced by the variable.
				# Then, we filter on whether or not its a reference to a type from a declaration.
				# If it is, we then check the referenced type to make sure its correct.
				network_profiling_payload_decl = find_cursor_recursive(
					cursor_matches_all(
						[cursor_is_of_kind, clang.cindex.CursorKind.DECL_REF_EXPR],
						[cursor_is_of_type, network_profiling_payload_enum.type]
					),
					variable_ref.referenced
				)


				if network_profiling_payload_decl is None:

					if outstanding_group:
						# Walk the parents and look for exposed MEMBER_REFs.
						# This will ensure that we're pointing at the
						# correct member variable (if a member variable is
						# being referenced).
						# Technically, we could do this in the query above.

						member_ref = find_parent(
							cursor_matches_all(
								[cursor_is_of_kind, clang.cindex.CursorKind.MEMBER_REF_EXPR],
								[lambda cursor: cursor.type.kind != clang.cindex.TypeKind.UNEXPOSED]
							),
							variable_ref
						)

						variable_ref = member_ref or variable_ref

						outstanding_group += [[method, call, variable_ref]]


				# If we found a payload type, we'll use that to denote the start of a new group.
				# First, we have to clean any outstanding ones.
				else:
					if outstanding_group:
						groups += [outstanding_group]

					outstanding_group = [[method, call, network_profiling_payload_decl]]


	if outstanding_group:
		groups += [outstanding_group]
		outstanding_group = None


	# Now we should have all of our groups set up,
	# so we can start our generation phase!
	# To kick things off, setup some templates.

	struct_template = '''
	USTRUCT()
	struct FNetworkProfilerType_{type} : public FNetworkProfilerType_Base
	{{
	public:
		GENERATED_BODY();
		NETWORK_PROFILER_BODY({type});

		FNetworkProfilerType_{type}({constructor_params}){init_list}
		{{
		}}

		~FNetworkProfilerType_{type}()
		{{
		}}
{get_methods}
		void Serialize(FArchive& Ar)
		{{
{serialize_body}
		}}

	private:
{member_decls}
	}};
	NETWORK_PROFILER_TYPE_TRAITS({type});
'''

	get_method_template = '''
		const {variable_type} Get{variable_name}() const
		{{
			return {variable_name};
		}}
'''

	member_decl_template = '''{variable_type} {variable_name};'''

	constructor_param_template = '''const {variable_type} In{variable_name}'''

	init_list_template = '''{variable_name}(In{variable_name})'''

	construct_template = '''FNetworkProfilerType_{type} {type}{inst_constructor_call};'''

	call_serialize_template = '''{type}.Serialize(*FileWriter);'''

	for group in groups:
		payload_method, payload_call, payload_var = group[0]
		group = group[1:]

		type = payload_var.spelling.replace('NPTYPE_', '')

		constructor_params = ''
		init_list = ''
		get_methods = ''
		serialize_body = ''
		member_decls=''
		inst_constructor_call = ''

		variables = (variable for method, call, variable in group)
		sorted_variables = sorted(variables, key=lambda variable: variable.type.get_size())

		variables_name_and_type = [
			(variable.spelling, variable.type.spelling.replace('const ', ''))
			for variable in sorted_variables
		]


		if len(variables_name_and_type) > 0:
			variable_templates = [
				{
					'constructor_param': constructor_param_template.format(variable_name=variable_name, variable_type=variable_type),
					'init_list': init_list_template.format(variable_name=variable_name, variable_type=variable_type),
					'get_method': get_method_template.format(variable_name=variable_name, variable_type=variable_type),
					'member_decl': member_decl_template.format(variable_name=variable_name, variable_type=variable_type),
				}
				for variable_name, variable_type in variables_name_and_type
			]

			constructor_params = ', '.join((
				variable_template['constructor_param']
				for variable_template in variable_templates
			))

			init_list = '\n\t\t\t, '.join((
				variable_template['init_list']
				for variable_template in variable_templates
			))

			init_list = '\n\t\t\t: ' + init_list

			get_methods = '\t\t'.join((
				variable_template['get_method']
				for variable_template in variable_templates
			))

			get_method = '\t\t' + get_methods

			member_decls = '\n\t\t'.join((
				variable_template['member_decl']
				for variable_template in variable_templates
			))

			member_decls = '\t\t' + member_decls

			# This bit is admittedly hacky.
			# We want to convert our calls into source.
			# We *could* just read the source code we're parsing,
			# grab the location, and then paste that in.
			# However, we may have done some munging.
			# For example, in FNetworkProfiler::FlushOutgoingBunches,
			# FSendBunchInfo / BunchInfo members are used, and instead we'll
			# want to use our own members.
			# Maybe a safer approach would be something like:
			#	1. Check to see if its operator<<.
			#		a. If it is, assume semantic Ar << {variable.spelling}
			#	2. Check to see if the call references a Member Function.
			#		a. If it does, and it's a member of FArchive,
			#			call it via Ar.{call.spelling}({variable.spelling})
			#		b. If it does, but it's not a member of FArchive,
			#			call it via {variable.spelling}.{call.spelling}(Ar)
			#	3. Check to see if it references an unbound function.
			#		a. If it does, figure out argument order, and place the Ar / Variable appropriately.
			#
			# Also, this fails to handle multiple variables that are passed into the same function...

			# Note, we use the group ordering here and not the sorted variable
			# ordering. This means that the generated body should be logically
			# identical to the first.

			def call_to_serialize(call, variable):
				if call.spelling == 'operator<<':
					return f'Ar << {variable.spelling};'

				elif 'IntPacked' in call.spelling:
					return f'Ar.{call.spelling}({variable.spelling});'

				else:
					return f'{variable.spelling}.{call.spelling}(Ar);'


			serialize_body = '\n\t\t\t'.join((
				call_to_serialize(call, variable)
				for method, call, variable in group
			))

			serialize_body = '\t\t\t' + serialize_body

			inst_constructor_call = '({})'.format(
				', '.join(
					variable_name
					for variable_name, variable_type in variables_name_and_type
				)
			)


		format_kwargs = {
			'constructor_params': constructor_params,
			'init_list': init_list,
			'get_methods': get_methods,
			'serialize_body': serialize_body,
			'member_decls': member_decls,
			'type': type,
			'inst_constructor_call': inst_constructor_call,
		}

		struct = struct_template.format(**format_kwargs)
		construct = construct_template.format(**format_kwargs)
		call_serialize = call_serialize_template.format(**format_kwargs)

		print(struct)
		print(construct)
		print(call_serialize)
		print(payload_call.location)
