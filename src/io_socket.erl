%%
%% Copyright (C) 2010-2014 by krasnop@bellsouth.net (Alexei Krasnopolski)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License. 
%%

%% @since 2011-3-24
%% @copyright 2010-2014 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://crasnopolski.com/]
%% @version {@version}
%% @doc The module is responsible for low level operation with socket. The reading from socket includes
%% decompression (if compress mode is set) and parsing input data stream to MySQL logical packets.
%% The writing operation forms from data lists logical packets and compress them if compress mode.

-module(io_socket).

%%
%% Include files
%%
-include("client_records.hrl").
-include("client_settings.hrl").

%%
%% Import modules
%%

%%
%% Exported Functions
%%
-export([
  receive_data/2, 
  send_single_packet/6
]).

%%
%% API Functions
%%
-type(socket()::port()).
-spec receive_data(Socket::socket(), Is_compressed::boolean()) -> list(#packet{}) | #mysql_error{}.

% % @spec receive_data(Socket::socket(), Is_compressed::boolean()) -> list(#packet{}) | #mysql_error{type=connection}
%% @doc Retreives data from socket and forms it as a list of binary mySQL packets.
%%
receive_data(Socket, Is_compressed) -> 
  try receive_data(Socket, get_data_from_socket(Socket), [], <<>>, false, Is_compressed)
  catch
    throw:Err when is_record(Err, mysql_error) -> Err;
    _:Err ->  #mysql_error{type=connection, source="io_socket:receive_data/2", message=Err}
  end.

%% Local Functions
%%
-spec receive_data(Socket::socket(), Chunk::binary(), PacketList::list(#packet{}), Last_uncmpss_chunk::binary(), Is_multi_packet::boolean(), Is_compressed::boolean()) -> list(#packet{}).
%% @spec receive_data(Socket::socket(), Chunk::binary(), PacketList::list(#packet{}), Last_uncmpss_chunk::binary(), Is_multi_packet::boolean(), Is_compressed::boolean()) -> list(#packet{})
%% @throws #mysql_error{type=connection}
%% @doc Recieves from the socket next portion of data. Chunk is unused part of data stream from previous
%% step, PacketList is accumulator of recognized binary packets from input data stream. If current packet is a part of
%% huge (or multi-packet) packet then parameter Is_multi_packet = true. If the connection to server set as compressed then 
%% parameter Is_compressed = true.
%%
receive_data(_, <<>>, [P | T], _, _, _) when is_list(T) -> [P | T];
receive_data(Socket, Chunk, PacketList, Last_uncmpss_chunk, Is_multi_packet, Is_compressed) ->
  case receive_packet_batch(Chunk, Last_uncmpss_chunk, PacketList, Is_multi_packet, Is_compressed) of
    {<<>>, <<>>, New_packet_list, false} -> 
      receive_data(Socket, <<>>, New_packet_list, <<>>, false, Is_compressed);
    {New_chunk, L, New_packet_list, Is_m_p} -> 
      B = <<New_chunk/binary, (get_data_from_socket(Socket))/binary>>,
      receive_data(Socket, B, New_packet_list, L, Is_m_p, Is_compressed)
  end.

%% @spec receive_packet_batch(Chunk::binary(), Last_uncmpss_chunk::binary(), PacketList::list(#packet{}), Is_multi_packet::boolean(), Is_compressed::boolean()) -> Result
%% @throws #mysql_error{type=connection}
%% @doc Recieves batch of uncompressed pakets from compressed/noncompressed packet (or multi packets).
receive_packet_batch(<<>>, Last_uncmpss_chunk, Packet_list, Is_multi_packet, true) -> 
  {<<>>, Last_uncmpss_chunk, Packet_list, Is_multi_packet};
receive_packet_batch(<<Packet_size:24/little-integer, _:8/integer, Uncompessed_size:24/little-integer, Packet:Packet_size/binary, New_chunk/binary>> = Chunk, 
                            Last_uncmpss_chunk, Packet_list, Is_multi_packet, true) when size(Chunk) > 4, size(New_chunk) >= 0 ->
%% debug
%  io:format(">>> receive_packet_batch(cmss) Chunk size: ~p Packet list size: ~p Last uncomp size: ~p MP: ~p~n"
%            "PacketSize: ~p Uncompessed_size: ~p New chunk size: ~p~n.~n", 
%            [size(Chunk), length(Packet_list), size(Last_uncmpss_chunk), Is_multi_packet, Packet_size, Uncompessed_size, size(New_chunk)]),
% debug
  Uncompressed_chunk = 
  case Uncompessed_size of 
    0 -> Packet;
    _ -> Uncomp_chunk = zlib:uncompress(Packet),
     if (size(Uncomp_chunk) =/= Uncompessed_size) -> 
        throw(#mysql_error{type=connection, source="io_socket:receive_single_packet/5", message="packet uncompressing error."});
        true -> Uncomp_chunk
     end
  end,
  {New_last_uncomp_chunk, _, New_packet_list, New_is_multi_packet} = 
    receive_packet_batch(<<Last_uncmpss_chunk/binary, Uncompressed_chunk/binary>>, <<>>, Packet_list, Is_multi_packet, false),
  receive_packet_batch(New_chunk, New_last_uncomp_chunk, New_packet_list, New_is_multi_packet, true);
receive_packet_batch(Chunk, Last_uncmpss_chunk, Packet_list, Is_multi_packet, true) ->
  {Chunk, Last_uncmpss_chunk, Packet_list, Is_multi_packet};

%% Recieves batch of packets from noncompressed packet (or multi packets).

receive_packet_batch(<<>>, _, Packet_list, Is_multi_packet, false) -> 
  {<<>>, <<>>, Packet_list, Is_multi_packet};
receive_packet_batch(
  <<Packet_size:24/little-integer, Packet_number:8/integer, Packet:Packet_size/binary, New_chunk/binary>> = Chunk, 
  _, Packet_list, Is_multi_packet, false) when size(Chunk) > 4, size(New_chunk) >= 0 ->
%% debug
%%  io:format(">>> receive_packet_batch(no cmss) Chunk size: ~p Packet list size: ~p MP: ~p~n"
%%            "PacketSize: ~p Pack #~p New chunk size: ~p Pack list size: ~p~n.~n", 
%%            [size(Chunk), length(Packet_list), Is_multi_packet, Packet_size, Packet_number, size(New_chunk), length(Packet_list)]),
%% debug
  case Is_multi_packet of
    false ->
      New_packet = #packet{seq_number = Packet_number, body = Packet},
      receive_packet_batch(New_chunk, <<>>, [New_packet | Packet_list], (Packet_size =:= ?MAX_PACKET_SIZE), false);
    true -> 
      [Multi_pack | Tail] = Packet_list,
      New_packet = #packet{seq_number = Packet_number, body = <<(Multi_pack#packet.body)/binary, Packet/binary>>},
      receive_packet_batch(New_chunk, <<>>, [New_packet | Tail], (Packet_size =:= ?MAX_PACKET_SIZE), false)
  end;
receive_packet_batch(Chunk, _, Packet_list, Is_multi_packet, false) ->
  {Chunk, <<>>, Packet_list, Is_multi_packet}.

-spec get_data_from_socket(Socket::socket()) -> binary().
%% @spec get_data_from_socket(Socket::socket()) -> binary()
%% @throws #mysql_error{type=tcp}
%% @doc Retrieves and returns a portion of binary data from socket.
%%
get_data_from_socket(Socket) -> get_data_from_socket(Socket, ?ACTIVE).

%% @spec get_data_from_socket(Socket::socket(), Mode::boolean()) -> binary()
%% @throws #mysql_error{type=tcp}
get_data_from_socket(Socket, false) ->
  case gen_tcp:recv(Socket, 0, ?RECV_TIMEOUT) of
    {ok, Bin} -> Bin;
    {error, closed} ->
        throw(#mysql_error{type = tcp, source = "io_socket:get_data_from_socket/1", message = "Socket is closed."});
    {error, Reason} ->
        throw(#mysql_error{type = tcp, source = "io_socket:get_data_from_socket/1", message = Reason})
  end;
get_data_from_socket(Socket, true) ->
  receive
    {tcp, Socket, Bin} -> Bin;
    {tcp_closed, Socket} ->
        throw(#mysql_error{type = tcp, source = "io_socket:get_data_from_socket/1", message = "Socket is closed."});
    {tcp_error, Socket, Reason} ->
        throw(#mysql_error{type = tcp, source = "io_socket:get_data_from_socket/1", message = Reason})
  after ?RECV_TIMEOUT ->
        throw(#mysql_error{type = tcp, source = "io_socket:get_data_from_socket/1", message = "Timeout"})
  end;
get_data_from_socket(_, _) ->
  throw(#mysql_error{type = tcp, source = "io_socket:get_data_from_socket/1", message = "bad Mode argument"}).

%% @spec send_single_packet(Socket::socket(), Packet_body::binary(), PacketSize::integer(), N::integer(), Is_multi_pack::boolean(), Is_compressed::boolean()) -> ok | #mysql_error{}
%% 
%% @doc Sends one single packet to socket output stream. If the packet has huge length the function splites 
%% the packet to chunks and processes these recursively.
%%
send_single_packet(Socket, Packet_body, PacketSize, N, false, false) ->
%  io:format(">>> send_single_packet ~p  ~p~n", [<<PacketSize:24/little-integer, N:8/integer>>, Packet_body]),
  case gen_tcp:send(Socket, <<PacketSize:24/little-integer, N:8/integer, Packet_body/binary>>) of
    ok -> ok;
    {error, Reason} -> #mysql_error{type = tcp, source="io_socket:send_single_packet/6", message = Reason}
  end;
%% if compressed
send_single_packet(Socket, Packet_body, PacketSize, N, false, true) when PacketSize > 50 ->
  CC = zlib:compress(<<PacketSize:24/little-integer, N:8/integer, Packet_body/binary>>),  
  Bin_body = <<(PacketSize + 4):24/little-integer, CC/binary>>,
  send_single_packet(Socket, Bin_body, size(CC), N, false, false);
send_single_packet(Socket, Packet_body, PacketSize, N, false, true) ->
  Bin_body = <<0:24/little-integer, PacketSize:24/little-integer, N:8/integer, Packet_body/binary>>,
  send_single_packet(Socket, Bin_body, PacketSize + 4, N, false, false);
%% if multi packet
send_single_packet(Socket, Packet_body, _, N, true, Is_compressed) ->
  <<Packet_head:(?MAX_PACKET_SIZE)/binary, Packet_tail/binary>> = Packet_body, 
  send_single_packet(Socket, Packet_head, ?MAX_PACKET_SIZE, N, false, Is_compressed),
  PacketSize = size(Packet_tail),
  send_single_packet(Socket, Packet_tail, PacketSize, N + 1, (PacketSize >= ?MAX_PACKET_SIZE), Is_compressed).

