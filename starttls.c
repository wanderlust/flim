/* TLSv1 filter for STARTTLS extension.

   Copyright (C) 1999 Daiki Ueno <ueno@ueda.info.waseda.ac.jp>

   Author: Daiki Ueno <ueno@ueda.info.waseda.ac.jp>
   Created: 1999-11-19                                                  
   Keywords: TLS, OpenSSL

   This file is part of FLIM (Faithful Library about Internet Message).

   This program is free software; you can redistribute it and/or modify 
   it under the terms of the GNU General Public License as published by 
   the Free Software Foundation; either version 2, or (at your option)  
   any later version.                                                   

   This program is distributed in the hope that it will be useful,      
   but WITHOUT ANY WARRANTY; without even the implied warranty of       
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the        
   GNU General Public License for more details.                         

   You should have received a copy of the GNU General Public License    
   along with GNU Emacs; see the file COPYING.  If not, write to the    
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,         
   Boston, MA 02111-1307, USA.                                          

*/

/*
  How to compile: (OpenSSL is required)
  
  gcc -I/usr/local/ssl/include -o starttls starttls.c \
    -L/usr/local/ssl/lib -lssl -lcrypto

*/

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>

/* OpenSSL library. */

#include <openssl/lhash.h>
#include <openssl/bn.h>
#include <openssl/err.h>
#include <openssl/pem.h>
#include <openssl/x509.h>
#include <openssl/ssl.h>

#ifdef HAVE_SOCKS_H
#include <socks.h>
#endif 

#include <sys/time.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <netdb.h>
#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <signal.h>

static SSL *tls_conn = NULL;
static int tls_fd;

static SSL_CTX *
tls_ssl_ctx_new (cert_file, key_file)
  const char *cert_file, key_file;
{
  SSL_CTX *tls_ctx;

  SSL_load_error_strings ();
  SSLeay_add_ssl_algorithms ();

  if ((tls_ctx = SSL_CTX_new (TLSv1_client_method())) == NULL)
    return NULL;

  SSL_CTX_set_options (tls_ctx, SSL_OP_ALL /* Work around all known bugs */); 

  if (cert_file) {
    if (SSL_CTX_use_certificate_file (tls_ctx, NULL, SSL_FILETYPE_PEM) <= 0)
      return NULL;
    if (SSL_CTX_use_PrivateKey_file (tls_ctx, NULL, SSL_FILETYPE_PEM) <= 0)
      return NULL;
    if (!SSL_CTX_check_private_key (tls_ctx))
      return NULL;
  }

  SSL_CTX_set_verify (tls_ctx, SSL_VERIFY_NONE, NULL);

  return tls_ctx;
}

static SSL *
tls_ssl_new(tls_ctx, s)
  SSL_CTX *tls_ctx;
  int s;
{
  SSL_SESSION *session;
  SSL_CIPHER *cipher;
  X509   *peer;

  if ((tls_conn = (SSL *) SSL_new (tls_ctx)) == NULL)
    return NULL;
  SSL_clear(tls_conn);

  if (!SSL_set_fd (tls_conn, s))
    return NULL;

  SSL_set_connect_state (tls_conn);

  if (SSL_connect (tls_conn) <= 0) {
    session = SSL_get_session (tls_conn);
    if (session) {
      SSL_CTX_remove_session (tls_ctx, session);
    }
    if (tls_conn!=NULL)
      SSL_free (tls_conn);
    return NULL;
  }

  return tls_conn;
}

static int
tls_connect (hostname, service)
     const char *hostname, *service;
{
  struct protoent *proto;
  struct addrinfo *in, hints;
  int server, false = 0;

  if ((proto = getprotobyname ("tcp")) == NULL)
    return -1;
  
  memset (&hints, 0, sizeof (hints));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_protocol = proto->p_proto;
  if (getaddrinfo (hostname, service, &hints, &in) < 0) 
    return -1;

  if ((server = socket (in->ai_family, in->ai_socktype, 0)) < 0)
    return -1;

  if (setsockopt (server, SOL_SOCKET, SO_KEEPALIVE,
		  (const char *) &false, sizeof (false))) 
    return -1;

  if (connect (server, in->ai_addr, in->ai_addrlen) < 0) {
    close (server);
    return -1;
  }

  return server;
}

static void
tls_negotiate (sig)
     int sig;
{
  SSL_CTX *tls_ctx;

  if ((tls_ctx = tls_ssl_ctx_new (NULL, NULL)) == NULL)
    return;

  tls_conn = tls_ssl_new (tls_ctx, tls_fd); /* Negotiation has done. */
}

int
main (argc, argv) 
  int argc;
  char **argv;
{
  int in = fileno (stdin), out = fileno (stdout), nbuffer, wrote;
  fd_set readfds, writefds;
  char buffer[BUFSIZ], *retry;
  struct sigaction act;

  if ((tls_fd = tls_connect (argv[1], argv[2])) < 0) {
    perror ("tls_connect");
    return 1;
  }

  memset (&act, 0, sizeof (act));
  act.sa_handler = tls_negotiate;
  sigemptyset (&act.sa_mask);
  act.sa_flags = SA_RESTART|SA_RESETHAND;
  sigaction (SIGALRM, &act, NULL);

  while (1) {
    FD_SET (tls_fd, &readfds);
    FD_SET (in, &readfds);
    if (select (tls_fd+1, &readfds, NULL, NULL, NULL) == -1 && 
	errno != EINTR ) {
      perror ("select");
      return 1;
    }
    if (FD_ISSET (in, &readfds)) {
      nbuffer = read (in, buffer, sizeof buffer -1);

      if (nbuffer == 0)
	goto finish;
      for (retry = buffer; nbuffer > 0; nbuffer -= wrote, retry += wrote) {
	FD_SET (tls_fd, &writefds);
	if (select (tls_fd+1, NULL, &writefds, NULL, NULL) == -1) {
	  perror ("select");
	  return 1;
	}
	if (tls_conn) 
	  wrote = SSL_write (tls_conn, retry, nbuffer);
	else
	  wrote = write (tls_fd, retry, nbuffer);
	if (wrote < 0) goto finish;
      }
    }
    if (FD_ISSET (tls_fd, &readfds)) {
      if (tls_conn)
	nbuffer = SSL_read (tls_conn, buffer, sizeof buffer -1);
      else
	nbuffer = read (tls_fd, buffer, sizeof buffer -1);
      if (nbuffer == 0)
	goto finish;
      for (retry = buffer; nbuffer > 0; nbuffer -= wrote, retry += wrote) {
	FD_SET (out, &writefds);
	if (select (out+1, NULL, &writefds, NULL, NULL) == -1) {
	  perror ("select");
	  return 1;
	}
	wrote = write (out, retry, nbuffer);
	if (wrote < 0) goto finish;
      }
    }
  }

 finish:
  close (in);
  close (out);
  
  return 0;
}
