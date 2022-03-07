import asyncio
import aiohttp
import sys
import json
import datetime
import argparse
import logging



servers = {"Riley": 12475,
           "Jaquez": 12476,
           "Juzang": 12477,
           "Campbell": 12478,
           "Bernard": 12479}

comms = {"Riley": ["Jaquez", "Juzang"],
         "Bernard": ["Jaquez", "Juzang", "Campbell"],
         "Juzang": ["Campbell", "Riley", "Bernard"],
         "Jaquez": ["Riley", "Bernard"],
         "Campbell": ["Bernard", "Juzang"]}

local_host = '127.0.0.1'

api_key = ''



# referred to TA Amit's slides, Python documentation examples & hint code repo
class Server:
    def __init__(self, name):
        self.name = name
        self.port = servers[name]
        self.clients = {}
        logging.info(f"Log file for server {name}")

        

    async def handle_connection(self, reader, writer):
        while not reader.at_eof():
            data = await reader.readline()
            message = data.decode()
            addr = writer.get_extra_info('peername')
            self.print_log(f"Received message from {addr}: {message}")

            response = await self.parse_message(message)
            if response:
                writer.write(response.encode())
                self.print_log(f"Sent message to {addr}: {response}")
            writer.close()
            self.print_log(f"Closing socket...")



    async def parse_message(self, message):
        command_table = {"IAMAT": self.handle_iamat,
                         "AT": self.handle_at,
                         "WHATSAT": self.handle_whatsat}
        message_list = [msg for msg in message.strip().split() if len(msg)]

        nfields = {"IAMAT": 4,
                   "AT": 6,
                   "WHATSAT": 4}
        
        # empty message
        if len(message_list) == 0:
            self.print_error(f"Invalid message: {message}")
            return f"? {message}"
        
        # invalid command
        if message_list[0] not in command_table:
            self.print_error(f"Invalid command: {command}")
            return f"? {message}"

        # incorrect number of fields for that command
        if (len(message_list) != nfields[message_list[0]]):
            self.print_error(f"Invalid number of fields: {nfields}")
            return f"? {message}"

        command = command_table.get(message_list[0])
        try:
            response = await command(*message_list[1:])
            return response
        except Exception as e:
            # error responding to message
            self.print_error(f"Error while responding to message: {e}")
            return f"? {message}"



    # IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1621464827.959498503
    async def handle_iamat(self, client_id, raw_coordinates, timestamp):
        coordinates = [x for x in raw_coordinates.replace('-', '+').split('+') if x != ""]

        if (len(coordinates) != 2 or
            not self.validnum(coordinates[0]) or
            not self.validnum(coordinates[1])):
            raise Exception(f"Invalid IAMAT coordinates: {raw_coordinates}")

        if not self.validnum(timestamp):
            raise Exception(f"Invalid IAMAT timestamp: {timestamp}")
        
        diff = datetime.datetime.now().timestamp() - float(timestamp)
        string_diff = '+'+str(diff) if diff > 0 else str(diff)
        response = f"AT {self.name} {string_diff} {client_id} {raw_coordinates} {timestamp}"

        self.clients[client_id] = {"timestamp":timestamp, "at":response}
        
        await self.flood(response)
        return response



    # AT Riley +0.263873386 kiwi.cs.ucla.edu +34.068930-118.445127 1621464827.959498503
    async def handle_at(self, server, diff, client_id, raw_coordinates, timestamp):
        if (not client_id in self.clients or
            timestamp > self.clients[client]["timestamp"]):
            at = f"AT {server} {diff} {client_id} {raw_coordinates} {timestamp}"
            self.clients[client] = {"timestamp":timestamp, "at": at}
            self.print_log(f"Received new update about client: {client_id}")
            await self.flood(at)

        else:
            self.print_log(f"Received redundant update about client: {client_id}")


            
    # WHATSAT kiwi.cs.ucla.edu 10 5
    async def handle_whatsat(self, client_id, radius, bound):
        if not client_id in self.clients:
            raise Exception(f"No information on client: {client_id}")

        if (not self.validnum(radius) or
            float(radius) < 0 or
            float(radius) > 50):
            raise Exception(f"Invalid radius: {radius}")

        if(not self.validnum(bound) or
           float(bound) < 0 or
           float(bound) > 20):
            raise Exception(f"Invalid upper bound: {bound}")

        at = self.clients[client_id]["at"]
        location = at.split()[4]
        places = (await self.places_info(location, radius, bound)).rstrip('\n')
        return f"{at}\n{places}\n\n"



    async def places_info(self, location, radius, bound):
        async with aiohttp.ClientSession() as session:
            url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={0}&location={1}&radius={2}'.format(api_key, self.parse_coords(location), radius)

            self.print_log(f"Looking for places at location: {location}")
            async with session.get(url) as response:
                places = await response.json(loads=json.loads)

            self.print_log(f'Found {len(places["results"])} places near location: {location}')
            if len(places["results"]) > int(bound):
                places["results"] = places["results"][:int(bound)]

            return str(json.dumps(places, indent=4))



    async def flood(self, message):
        for s in comms[self.name]:
            try:
                _, writer = await asyncio.open_connection(local_host, servers[s])
                writer.write(message.encode())
                self.print_log(f"Flooding to {s}...")
                writer.close()
                await writer.wait_closed()
                self.print_log(f"Closing connection...")
                
            except Exception as e:
                self.print_log(f"Could not connect to {s}: {e}")



    async def run_forever(self):
        server = await asyncio.start_server(self.handle_connection, local_host, self.port)
        self.print_log(f"Serving on {server.sockets[0].getsockname()}")
        async with server:
            await server.serve_forever()



    def run(self):
        try:
            asyncio.run(self.run_forever())
        except KeyboardInterrupt:
            self.print_log(f"Server {self.name} closed")


            
    def parse_coords(self, raw_coordinates):
        m = max(raw_coordinates.rfind('+'), raw_coordinates.rfind('-'))
        return f"{raw_coordinates[:m]},{raw_coordinates[m:]}"



    def validnum(self, string):
        try:
            float(string)
            return True
        except ValueError:
            return False


        
    def print_log(self, msg):
        print(msg)
        logging.info(msg)



    def print_error(self, msg):
        print(msg)
        logging.error(msg)



if __name__ == "__main__":
    parser = argparse.ArgumentParser("Proxy Herd Server")
    parser.add_argument("server_name", type=str, help="required server name input")
    args = parser.parse_args()
    if not args.server_name in servers:
        print(f"Invalid server name: {args.server_name}")
        exit()
    logging.basicConfig(filename=f"{args.server_name}.log", format="%(asctime)s %(levelname)s %(message)s", level=logging.DEBUG)
    server = Server(args.server_name)
    server.run()                                  
