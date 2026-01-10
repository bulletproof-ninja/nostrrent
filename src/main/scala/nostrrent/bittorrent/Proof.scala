package nostrrent.bittorrent

import nostrrent.nostr.NostrSignature
import scala.util.Try

object Proof:
  def toComment(sig: NostrSignature): String =
    s"${sig.npub}:${sig.hashSigHex}"
  def fromComment(comment: String): Option[NostrSignature] =
    if comment == null then None
    else comment.split(":") match
      case Array(npub, hashSig) => Try { NostrSignature(npub, hashSig) }.toOption
      case _ => None
